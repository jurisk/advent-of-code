package jurisk.utils

object Geometry {
  implicit class StringOps(s: String) {
    def toX: X = X(s.toInt)
    def toY: Y = Y(s.toInt)
  }

  // X and Y do not "extends AnyVal" on purpose because of type erasure issues
  final case class X(value: Int) {
    def *(n: Int): X = X(value * n)
    def /(n: Int): X = X(value / n)

    def <(other: X): Boolean = value < other.value
    def >(other: X): Boolean = value > other.value

    def +(other: X): X   = this + other.value
    def -(other: X): X   = this - other.value
    def +(other: Int): X = X(value + other)
    def -(other: Int): X = X(value - other)
    def abs: Int         = Math.abs(value)

    override def toString: String = value.toString
  }

  final case class Y(value: Int) {
    def *(n: Int): Y = Y(value * n)
    def /(n: Int): Y = Y(value / n)

    def <(other: Y): Boolean = value < other.value
    def >(other: Y): Boolean = value > other.value

    def +(other: Y): Y   = this + other.value
    def -(other: Y): Y   = this - other.value
    def +(other: Int): Y = Y(value + other)
    def -(other: Int): Y = Y(value - other)
    def abs: Int         = Math.abs(value)

    override def toString: String = value.toString
  }

  final case class Coords2D(x: X, y: Y) {
    def +(other: Coords2D): Coords2D =
      Coords2D(x + other.x, y + other.y)

    def -(other: Coords2D): Coords2D =
      Coords2D(x - other.x, y - other.y)

    def manhattanDistanceToOrigin: Int =
      Math.abs(x.value) + Math.abs(y.value)

    def manhattanDistance(other: Coords2D): Int =
      (this - other).manhattanDistanceToOrigin

    def neighbours(includeDiagonal: Boolean): List[Coords2D] = {
      val directions = if (includeDiagonal) {
        Direction2D.AllDirections
      } else {
        Direction2D.MainDirections
      }

      directions.map(d => this + d.diff)
    }

    override def toString: String = s"($x, $y)"
  }

  object Coords2D {
    val Zero: Coords2D = Coords2D.of(0, 0)

    def of(x: Int, y: Int): Coords2D =
      Coords2D(X(x), Y(y))

    def boundingBoxInclusive(coords: Seq[Coords2D]): Area2D = {
      val minX = coords.map(_.x.value).min
      val minY = coords.map(_.y.value).min
      val maxX = coords.map(_.x.value).max
      val maxY = coords.map(_.y.value).max
      Area2D(X(minX), Y(minY), maxX - minX + 1, maxY - minY + 1)
    }
  }

  sealed trait Direction2D {
    def diff: Coords2D
  }

  object Direction2D {
    val MainDirections: List[Direction2D]     = N :: S :: W :: E :: Nil
    val DiagonalDirections: List[Direction2D] = NW :: NE :: SW :: SE :: Nil
    val AllDirections: List[Direction2D]      = MainDirections ::: DiagonalDirections

    case object N extends Direction2D {
      val diff: Coords2D = Coords2D.of(0, -1)
    }

    case object NE extends Direction2D {
      val diff: Coords2D = N.diff + E.diff
    }

    case object NW extends Direction2D {
      val diff: Coords2D = N.diff + W.diff
    }

    case object S extends Direction2D {
      val diff: Coords2D = Coords2D.of(0, +1)
    }

    case object SE extends Direction2D {
      val diff: Coords2D = S.diff + E.diff
    }

    case object SW extends Direction2D {
      val diff: Coords2D = S.diff + W.diff
    }

    case object W extends Direction2D {
      val diff: Coords2D = Coords2D.of(-1, 0)
    }

    case object E extends Direction2D {
      val diff: Coords2D = Coords2D.of(1, 0)
    }

    def parseUDLR(s: String): Direction2D = s match {
      case "U" => N
      case "D" => S
      case "L" => W
      case "R" => E
      case _   => sys.error(s"Unrecognized main direction: $s")
    }
  }

  final case class Area2D(left: X, top: Y, width: Int, height: Int) {
    def pointSet: Set[Coords2D] = {
      (0 until width) flatMap { w =>
        (0 until height) map { h => Coords2D(left + w, top + h) }
      }
    }.toSet
  }

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

    def mapByCoords[B](f: Coords2D => B): Field2D[B] = map { case (c, _) =>
      f(c)
    }
    def mapByValues[B](f: T => B): Field2D[B]        = map { case (_, v) => f(v) }

    def at(c: Coords2D): Option[T]       =
      data.lift(c.y.value).flatMap(_.lift(c.x.value))
    private def atUnsafe(c: Coords2D): T =
      at(c).getOrElse(sys.error(s"Coords2D $c are invalid"))

    def allCoords: Seq[Coords2D] =
      yIndices flatMap { y =>
        xIndices map { x =>
          Coords2D(x, y)
        }
      }

    def values: Iterable[T] = data.flatten

    def row(y: Y): Vector[T]    = data(y.value)
    def column(x: X): Vector[T] = data.map(_(x.value))

    def count(p: T => Boolean): Int =
      values.count(p)
  }

  object Field2D {
    def toDebugRepresentation(field: Field2D[Char]): String =
      (field.yIndices map { y =>
        field.xIndices.map { x =>
          field.atUnsafe(Coords2D(x, y))
        }.mkString
      }).map(_ + '\n').mkString

    def parseFromLines[T](lines: List[String], parser: Char => T): Field2D[T] =
      Field2D(lines.toVector.map(_.map(parser).toVector))
  }
}
