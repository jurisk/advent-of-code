package jurisk.geometry

import cats.Functor

final case class Field2D[T](data: Vector[Vector[T]], topLeft: Coords2D = Coords2D.Zero) {
  val width: Int  = data.head.length
  val height: Int = data.length

  private def xIndices: Seq[X] = (0 until width).map(x => X(x) + topLeft.x)
  private def yIndices: Seq[Y] = (0 until height).map(y => Y(y) + topLeft.y)

  def mapByCoordsWithValues[B](f: (Coords2D, T) => B): Field2D[B] = Field2D {
    yIndices.toVector map { y =>
      xIndices.toVector map { x =>
        val coords = Coords2D(x, y)
        f(coords, this(coords))
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

  def get(c: Coords2D): Option[T] = at(c)

  def at(c: Coords2D): Option[T] =
    data.lift(c.y.value - topLeft.y.value).flatMap(_.lift(c.x.value - topLeft.x.value))

  def apply(c: Coords2D): T =
    at(c).getOrElse(sys.error(s"Coords2D $c are invalid"))

  def updatedAtUnsafe(c: Coords2D, newValue: T): Field2D[T] = {
    val yIdx = c.y.value - topLeft.y.value
    copy(
      data = data.updated(yIdx, data(yIdx).updated(c.x.value - topLeft.x.value, newValue))
    )
  }

  def isValidCoordinate(c: Coords2D): Boolean = at(c).isDefined

  def adjacent4(c: Coords2D): List[Coords2D] = neighboursFor(c, includeDiagonal = false)
  def adjacent8(c: Coords2D): List[Coords2D] = neighboursFor(c, includeDiagonal = true)

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
        c -> this(c)
      }
    }

  def row(y: Y): Vector[T]    = data(y.value - topLeft.y.value)
  def coordsForRow(y: Y): List[Coords2D] = xIndices.toList map { x =>
    Coords2D(x, y)
  }

  def column(x: X): Vector[T] = data.map(_(x.value - topLeft.x.value))
  def coordsForColumn(x: X): List[Coords2D] = yIndices.toList map { y =>
    Coords2D(x, y)
  }

  def count(p: T => Boolean): Int =
    values.count(p)

  def createSuccessorsFunction(
    canGoPredicate: (T, T) => Boolean,
    includeDiagonal: Boolean,
  ): Coords2D => List[Coords2D] = { (c: Coords2D) =>
    neighboursFor(c, includeDiagonal = includeDiagonal) filter { n =>
      val thisSquare  = this(c)
      val otherSquare = this(n)
      canGoPredicate(thisSquare, otherSquare)
    }
  }
}

object Field2D {
  implicit val functorField2D: Functor[Field2D] = new Functor[Field2D] {
    override def map[A, B](fa: Field2D[A])(f: A => B): Field2D[B] =
      fa.mapByCoordsWithValues { case (_, v) => f(v) }
  }

  def ofSize[T](width: Int, height: Int, initialValue: T, topLeft: Coords2D = Coords2D.Zero): Field2D[T] =
    Field2D(
      Vector.fill(height)(Vector.fill(width)(initialValue)),
      topLeft,
    )

  def toDebugRepresentation(field: Field2D[Char]): String = mergeSeqSeqChar(
    field.yIndices map { y =>
      field.xIndices.map { x =>
        field(Coords2D(x, y))
      }
    }
  )

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
