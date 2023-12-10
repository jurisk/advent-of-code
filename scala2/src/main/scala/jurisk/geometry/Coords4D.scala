package jurisk.geometry

import jurisk.utils.Parsing.StringOps

final case class Coords4D(x: Int, y: Int, z: Int, t: Int) {
  def manhattanDistance(other: Coords4D): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z) + Math
      .abs(t - other.t)
}

object Coords4D {
  def parse(s: String): Coords4D =
    s match {
      case s"$x,$y,$z,$t" =>
        Coords4D(x.trim.toInt, y.trim.toInt, z.trim.toInt, t.trim.toInt)
      case _              => s.failedToParse("Coords4D")
    }
}
