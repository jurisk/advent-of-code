package jurisk.geometry

final case class Field2D[T](data: Vector[Vector[T]]) {
  val width: Int  = data.head.length
  val height: Int = data.length

  private def xIndices: Seq[X] = (0 until width).map(X)
  private def yIndices: Seq[Y] = (0 until height).map(Y)

  def map[B](f: (Coords2D, T) => B): Field2D[B] = Field2D {
    yIndices.toVector map { y =>
      xIndices.toVector map { x =>
        val coords = Coords2D(x, y)
        f(coords, atUnsafe(coords))
      }
    }
  }

  def filterCoordsByValue(p: T => Boolean): List[Coords2D] = entries.filter { case (_, v) =>
    p(v)
  }.map { case (c, _) =>
    c
  }.toList

  def mapByCoords[B](f: Coords2D => B): Field2D[B] = map { case (c, _) =>
    f(c)
  }
  def mapByValues[B](f: T => B): Field2D[B]        = map { case (_, v) => f(v) }

  def at(c: Coords2D): Option[T] =
    data.lift(c.y.value).flatMap(_.lift(c.x.value))

  def atUnsafe(c: Coords2D): T =
    at(c).getOrElse(sys.error(s"Coords2D $c are invalid"))

  def updatedAtUnsafe(c: Coords2D, newValue: T): Field2D[T] =
    Field2D(
      data.updated(c.y.value, data(c.y.value).updated(c.x.value, newValue))
    )

  def isValidCoordinate(c: Coords2D): Boolean = at(c).isDefined

  def neighboursFor(c: Coords2D, includeDiagonal: Boolean): List[Coords2D] = c.neighbours(includeDiagonal).filter(isValidCoordinate)

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

  def row(y: Y): Vector[T]    = data(y.value)
  def column(x: X): Vector[T] = data.map(_(x.value))

  def count(p: T => Boolean): Int =
    values.count(p)


  def createSuccessorsFunction(
    canGoPredicate: (T, T) => Boolean,
    includeDiagonal: Boolean,
  ): Coords2D => List[Coords2D] = {
    (c: Coords2D) =>
      neighboursFor(c, includeDiagonal = includeDiagonal) filter { n =>
        val thisSquare = atUnsafe(c)
        val otherSquare = atUnsafe(n)
        canGoPredicate(thisSquare, otherSquare)
      }
  }
}

object Field2D {
  def ofSize[T](width: Int, height: Int, initialValue: T): Field2D[T] =
    Field2D(
      Vector.fill(height)(Vector.fill(width)(initialValue))
    )

  def toDebugRepresentation(field: Field2D[Char]): String = mergeSeqSeqChar(
    field.yIndices map { y =>
      field.xIndices.map { x =>
        field.atUnsafe(Coords2D(x, y))
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
