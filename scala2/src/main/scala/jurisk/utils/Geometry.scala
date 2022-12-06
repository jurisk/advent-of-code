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
}
