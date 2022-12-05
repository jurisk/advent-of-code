package jurisk.utils

object Geometry {
  implicit class StringOps(s: String) {
    def toX: X = X(s.toInt)
    def toY: Y = Y(s.toInt)
  }

  final case class X(value: Int) extends AnyVal {
    def +(other: Int): X = X(value + other)
  }

  final case class Y(value: Int) extends AnyVal {
    def +(other: Int): Y = Y(value + other)
  }

  final case class Coords2D(x: X, y: Y)

  final case class Area2D(left: X, top: Y, width: Int, height: Int) {
    def pointSet: Set[Coords2D] = {
      (0 until width) flatMap { w =>
        (0 until height) map { h => Coords2D(left + w, top + h) }
      }
    }.toSet
  }
}
