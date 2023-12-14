package jurisk.geometry

import cats.Eval
import cats.Foldable
import cats.Functor
import cats.implicits._
import jurisk.algorithms.pathfinding.Bfs
import jurisk.utils.Parsing.StringOps

final case class Field2D[T](
  data: Vector[Vector[T]],
  topLeft: Coords2D = Coords2D.Zero,
) {
  val width: Int  = data.head.length
  val height: Int = data.length

  def bottomRight: Coords2D = topLeft + Coords2D.of(width - 1, height - 1)

  def xIndices: Seq[Int] = (0 until width).map(x => x + topLeft.x)
  def yIndices: Seq[Int] = (0 until height).map(y => y + topLeft.y)

  def rotate(rotation: Rotation): Field2D[T] = {
    val newData = rotation match {
      case Rotation.Left90     => data.map(_.reverse).transpose
      case Rotation.NoRotation => data
      case Rotation.Right90    => data.transpose.map(row => row.reverse)
      case Rotation.TurnAround => data.map(_.reverse).reverse
    }

    Field2D(newData)
  }

  def mapByCoordsWithValues[B](f: (Coords2D, T) => B): Field2D[B] = Field2D {
    yIndices.toVector map { y =>
      xIndices.toVector map { x =>
        val coords = Coords2D.of(x, y)
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

  def modifyUnsafe(c: Coords2D, f: T => T): Field2D[T] =
    at(c) match {
      case Some(value) => updatedAtUnsafe(c, f(value))
      case None        => s"Coordinate $c is out of bounds in `modifyUnsafe`".fail
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
        Coords2D.of(x, y)
      }
    }

  def values: Iterable[T] = data.flatten

  def valuesAndCoords: Seq[(Coords2D, T)] =
    yIndices flatMap { y =>
      xIndices map { x =>
        val c = Coords2D.of(x, y)
        c -> atUnsafe(c)
      }
    }

  def entries: Seq[(Coords2D, T)] =
    yIndices flatMap { y =>
      xIndices map { x =>
        val c = Coords2D.of(x, y)
        c -> atUnsafe(c)
      }
    }

  def row(y: Int): Vector[T]               = data(y - topLeft.y)
  def coordsForRow(y: Int): List[Coords2D] = xIndices.toList map { x =>
    Coords2D.of(x, y)
  }

  def rows: List[Vector[T]] = data.toList

  def columns: List[Vector[T]] = for {
    columnIdx <- (0 until width).toList
  } yield column(columnIdx)

  def column(x: Int): Vector[T]               = data.map(_(x - topLeft.x))
  def coordsForColumn(x: Int): List[Coords2D] = yIndices.toList map { y =>
    Coords2D.of(x, y)
  }

  def firstRowValues: Vector[T] = row(0)
  def lastRowValues: Vector[T]  = row(height - 1)

  def firstColumnValues: Vector[T] = column(0)
  def lastColumnValues: Vector[T]  = column(width - 1)

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

  /** Note - all the fields returned by `f` must have the same shape.
    */
  def flatMap[U](f: T => Field2D[U]): Field2D[U] = {
    val newData = data.flatMap { row =>
      val transformedRows = row.map(f(_).data)
      if (transformedRows.isEmpty) Vector.empty
      else transformedRows.transpose.map(_.flatten)
    }

    Field2D(newData)
  }

  def chunkIntoSubfields(width: Int, height: Int): Field2D[Field2D[T]] = {
    assert(
      data.nonEmpty && data.forall(_.length == data.head.length),
      "Irregular dimensions in Field2D",
    )
    assert(
      data.head.length % width == 0,
      s"Width $width does not evenly divide the row length",
    )
    assert(
      data.length      % height == 0,
      s"Height $height does not evenly divide the number of rows",
    )

    val rowChunks        = data.map(row => row.grouped(width).toVector)
    val transposedChunks = rowChunks.transpose
    val chunkedRows      =
      transposedChunks.map(column => column.grouped(height).toVector).transpose
    val subfields        = chunkedRows.map(_.map(Field2D(_)))
    Field2D(subfields)
  }

  def reverseRows: Field2D[T]    = Field2D(data.reverse)
  def reverseColumns: Field2D[T] = Field2D(data.map(_.reverse))

  def topRows(rows: Int): Field2D[T]        = Field2D(data.take(rows))
  def leftColumns(columns: Int): Field2D[T] = Field2D(data.map(_.take(columns)))

  def bottomRows(rows: Int): Field2D[T]      = Field2D(data.takeRight(rows))
  def rightColumns(columns: Int): Field2D[T] = Field2D(
    data.map(_.takeRight(columns))
  )
}

object Field2D {
  implicit val functorInstance: Functor[Field2D] = new Functor[Field2D] {
    override def map[A, B](fa: Field2D[A])(f: A => B): Field2D[B] =
      fa.mapByCoordsWithValues { case (_, v) => f(v) }
  }

  implicit val foldableInstance: Foldable[Field2D] = new Foldable[Field2D] {
    override def foldLeft[A, B](fa: Field2D[A], b: B)(f: (B, A) => B): B =
      fa.data.foldLeft(b)((acc, row) => row.foldLeft(acc)(f))

    override def foldRight[A, B](fa: Field2D[A], lb: Eval[B])(
      f: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      fa.data.foldRight(lb)((row, acc) => row.foldRight(acc)(f))
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

  def floodFillField[T](
    field: Field2D[T],
    seed: Coords2D,
    f: (T, T) => Boolean,
    mark: T,
  ): Field2D[T] = {
    val reachable =
      Bfs.bfsReachable[Coords2D](
        seed,
        from =>
          field.adjacent4(from).filter { to =>
            f(field.atUnsafe(from), field.atUnsafe(to))
          },
      )

    // We lack bulk update feature, so this will have to do
    reachable.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c, mark)
    }
  }

  def toDebugRepresentation(field: Field2D[Char]): String = mergeSeqSeqChar(
    field.yIndices map { y =>
      field.xIndices.map { x =>
        field.atUnsafe(Coords2D.of(x, y))
      }
    }
  )

  def printCharField(field: Field2D[Char]): Unit =
    printField[Char](field, identity, none)

  def printField[T](
    field: Field2D[T],
    toChar: T => Char,
    intro: Option[String] = None,
  ): Unit = {
    intro foreach println
    val charField      = field.map(toChar)
    val representation = toDebugRepresentation(charField)
    println(representation)
    println()
  }

  def printBooleanField(field: Field2D[Boolean]): Unit =
    printField(field, visualizeBoolean)

  def parse[T](data: String, parser: Char => T): Field2D[T] =
    parseFromLines(
      data
        .split("\\R") // not splitting on '\n' because it failed in Windows
        .filter(_.nonEmpty)
        .toList,
      parser,
    )

  def parseCharField(data: String): Field2D[Char] = parse(data, identity)
  def parseDigitField(data: String): Field2D[Int] = parse(data, _ - '0')
  def parseBooleanField(
    data: String,
    falseChar: Char = '.',
    trueChar: Char = '#',
  ): Field2D[Boolean] = parse(
    data,
    {
      case `falseChar` => false
      case `trueChar`  => true
      case ch          => s"Unrecognized character: $ch".fail
    },
  )

  private def parseFromLines[T](
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
