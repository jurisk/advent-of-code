package jurisk.geometry

import cats.Functor
import cats.implicits.toFunctorOps
import jurisk.utils.Parsing.StringOps

final case class Field2D[T](
  private val data: Vector[Vector[T]],
  topLeft: Coords2D = Coords2D.Zero,
) {
  val width: Int  = data.head.length
  val height: Int = data.length

  def bottomRight: Coords2D = topLeft + Coords2D.of(width - 1, height - 1)

  def xIndices: Seq[Int] = (0 until width).map(x => x + topLeft.x)
  def yIndices: Seq[Int] = (0 until height).map(y => y + topLeft.y)

  def mapByCoordsWithValues[B](f: (Coords2D, T) => B): Field2D[B] = Field2D {
    yIndices.toVector map { y =>
      xIndices.toVector map { x =>
        val coords = Coords2D(x, y)
        f(coords, atUnsafe(coords))
      }
    }
  }

  def filterCoordsByValue(p: T => Boolean): List[Coords2D] = entries
    .filter { case (_, v) =>
      p(v)
    }
    .map { case (c, _) =>
      c
    }
    .toList

  def mapByCoords[B](f: Coords2D => B): Field2D[B] = mapByCoordsWithValues {
    case (c, _) =>
      f(c)
  }

  def at(c: Coords2D): Option[T] =
    data
      .lift(c.y - topLeft.y)
      .flatMap(_.lift(c.x - topLeft.x))

  def atOrElse(c: Coords2D, default: => T): T =
    at(c) getOrElse default

  private def atUnsafe(c: Coords2D): T =
    at(c) getOrElse s"Coords2D $c are invalid".fail

  def updatedAtUnsafe(c: Coords2D, newValue: T): Field2D[T] = {
    val yIdx = c.y - topLeft.y
    copy(
      data = data.updated(
        yIdx,
        data(yIdx).updated(c.x - topLeft.x, newValue),
      )
    )
  }

  def conditionalUpdate(
    c: Coords2D,
    condition: T => Boolean,
    valueIfTrue: => T,
  ): Field2D[T] =
    if (at(c).exists(condition)) {
      updatedAtUnsafe(c, valueIfTrue)
    } else {
      this
    }

  def isValidCoordinate(c: Coords2D): Boolean = at(c).isDefined

  def adjacent4(c: Coords2D): List[Coords2D] =
    neighboursFor(c, includeDiagonal = false)

  def adjacent4Where(c: Coords2D, predicate: T => Boolean): List[Coords2D] =
    adjacent4(c) filter { n =>
      at(n) exists predicate
    }

  def adjacent4Values(c: Coords2D): List[T] =
    adjacent4(c).flatMap(at)

  def adjacent8(c: Coords2D): List[Coords2D] =
    neighboursFor(c, includeDiagonal = true)

  def adjacent8Where(c: Coords2D, predicate: T => Boolean): List[Coords2D] =
    adjacent8(c) filter { n =>
      at(n) exists predicate
    }

  def adjacent8Values(c: Coords2D): List[T] =
    adjacent8(c).flatMap(at)

  def neighboursFor(c: Coords2D, includeDiagonal: Boolean): List[Coords2D] =
    c.neighbours(includeDiagonal).filter(isValidCoordinate)

  def allCoords: Seq[Coords2D] =
    yIndices flatMap { y =>
      xIndices map { x =>
        Coords2D(x, y)
      }
    }

  def values: Iterable[T] = data.flatten

  def entries: Seq[(Coords2D, T)] =
    yIndices flatMap { y =>
      xIndices map { x =>
        val c = Coords2D(x, y)
        c -> atUnsafe(c)
      }
    }

  def row(y: Int): Vector[T]               = data(y - topLeft.y)
  def coordsForRow(y: Int): List[Coords2D] = xIndices.toList map { x =>
    Coords2D(x, y)
  }

  def rows: List[Vector[T]] = data.toList

  def columns: List[Vector[T]] = for {
    columnIdx <- (0 until width).toList
  } yield column(columnIdx)

  def column(x: Int): Vector[T]               = data.map(_(x - topLeft.x))
  def coordsForColumn(x: Int): List[Coords2D] = yIndices.toList map { y =>
    Coords2D(x, y)
  }

  def firstRowValues: Vector[T] = row(0)
  def lastRowValues: Vector[T]  = row(height - 1)

  def firstColumnValues: Vector[T] = column(0)
  def lastColumnValues: Vector[T]  = column(width - 1)

  def count(p: T => Boolean): Int =
    values count p

  def createSuccessorsFunction(
    canGoPredicate: (T, T) => Boolean,
    includeDiagonal: Boolean,
  ): Coords2D => List[Coords2D] = { (c: Coords2D) =>
    neighboursFor(c, includeDiagonal = includeDiagonal) filter { n =>
      val thisSquare  = atUnsafe(c)
      val otherSquare = atUnsafe(n)
      canGoPredicate(thisSquare, otherSquare)
    }
  }
}

object Field2D {
  implicit val functorField2D: Functor[Field2D] = new Functor[Field2D] {
    override def map[A, B](fa: Field2D[A])(f: A => B): Field2D[B] =
      fa.mapByCoordsWithValues { case (_, v) => f(v) }
  }

  def ofSize[T](
    width: Int,
    height: Int,
    initialValue: T,
    topLeft: Coords2D = Coords2D.Zero,
  ): Field2D[T] =
    Field2D(
      Vector.fill(height)(Vector.fill(width)(initialValue)),
      topLeft,
    )

  def forArea[T](boundingBox: Area2D, initialValue: T): Field2D[T] =
    ofSize(
      boundingBox.width,
      boundingBox.height,
      initialValue,
      boundingBox.topLeft,
    )

  def toDebugRepresentation(field: Field2D[Char]): String = mergeSeqSeqChar(
    field.yIndices map { y =>
      field.xIndices.map { x =>
        field.atUnsafe(Coords2D(x, y))
      }
    }
  )

  def printField[T](
    intro: Option[String],
    field: Field2D[T],
    toChar: T => Char,
  ): Unit = {
    intro foreach println
    val charField      = field.map(toChar)
    val representation = Field2D.toDebugRepresentation(charField)
    println(representation)
    println()
  }

  def parseFromString[T](data: String, parser: Char => T): Field2D[T] =
    parseFromLines(
      data
        .split("\\R")
        .filter(_.nonEmpty)
        .toList, // not splitting on '\n' because it failed in Windows
      parser,
    )

  def parseFromLines[T](
    lines: List[String],
    parser: Char => T,
    padIfNotEnoughWidthWith: Char = ' ',
  ): Field2D[T] = {
    val width = lines.map(_.length).max
    Field2D(
      lines.toVector.map { line =>
        val paddedLine = line.padTo(width, padIfNotEnoughWidthWith)
        paddedLine.map(parser).toVector
      }
    )
  }
}
