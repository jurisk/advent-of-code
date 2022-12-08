package jurisk.utils

object Geometry {
  implicit class StringOps(s: String) {
    def toX: X = X(s.toInt)
    def toY: Y = Y(s.toInt)
  }

  final case class X(value: Int) extends AnyVal {
    def +(other: X): X            = X(value + other.value)
    def -(other: X): X            = X(value - other.value)
    override def toString: String = value.toString
  }

  final case class Y(value: Int) extends AnyVal {
    def +(other: Y): Y            = Y(value + other.value)
    def -(other: Y): Y            = Y(value - other.value)
    override def toString: String = value.toString
  }

  final case class Coords2D(x: X, y: Y) {
    def -(other: Coords2D): Coords2D =
      Coords2D(x - other.x, y - other.y)

    def manhattanDistanceToOrigin: Int =
      Math.abs(x.value) + Math.abs(y.value)

    def manhattanDistance(other: Coords2D): Int =
      (this - other).manhattanDistanceToOrigin

    override def toString: String = s"($x, $y)"
  }

  object Coords2D {
    def of(x: Int, y: Int): Coords2D =
      Coords2D(X(x), Y(y))

    def boundingBox(coords: Seq[Coords2D]): Area2D = {
      val minX = coords.map(_.x.value).min
      val minY = coords.map(_.y.value).min
      val maxX = coords.map(_.x.value).max
      val maxY = coords.map(_.y.value).max
      Area2D(X(minX), Y(minY), maxX - minX + 1, maxY - minY + 1)
    }
  }

  final case class Area2D(left: X, top: Y, width: Int, height: Int) {
    def pointSet: Set[Coords2D] = {
      (0 until width) flatMap { w =>
        (0 until height) map { h => Coords2D(left + X(w), top + Y(h)) }
      }
    }.toSet
  }

  final case class Field2D[T](data: Vector[Vector[T]]) {
    val width: Int  = data.head.length
    val height: Int = data.length

    def map[B](f: (Coords2D, T) => B): Field2D[B] = Field2D {
      (0 until height).toVector map { y =>
        (0 until width).toVector map { x =>
          f(Coords2D.of(x, y), at(x, y))
        }
      }
    }

    def mapByCoords[B](f: Coords2D => B): Field2D[B] = map { case (c, _) =>
      f(c)
    }
    def mapByValues[B](f: T => B): Field2D[B]        = map { case (_, v) => f(v) }

    def at(c: Coords2D): T    = at(c.x.value, c.y.value)
    def at(x: Int, y: Int): T = data(y)(x)

    def allCoords: List[Coords2D] =
      ((0 until height) flatMap { y =>
        (0 until width) map { x =>
          Coords2D.of(x, y)
        }
      }).toList

    def toValuesList: List[T] = data.flatten.toList

    def row(y: Y): Vector[T]    = data(y.value)
    def column(x: X): Vector[T] = data.map(_(x.value))

    def count(p: T => Boolean): Int =
      toValuesList.count(p)
  }

  object Field2D {
    def debugPrint(field: Field2D[Char]): String =
      ((0 until field.height) map { y =>
        (0 until field.width).map { x =>
          field.at(x, y)
        }.mkString
      }).map(_ + '\n').mkString

    def parseFromLines[T](lines: List[String], parser: Char => T): Field2D[T] =
      Field2D(lines.toVector.map(_.map(parser).toVector))
  }
}
