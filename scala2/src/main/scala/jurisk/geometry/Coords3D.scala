package jurisk.geometry

import cats.implicits._

final case class Coords3D(x: Int, y: Int, z: Int) {
  def adjacent6: List[Coords3D] = List(
    Coords3D(x + 1, y, z),
    Coords3D(x - 1, y, z),
    Coords3D(x, y + 1, z),
    Coords3D(x, y - 1, z),
    Coords3D(x, y, z + 1),
    Coords3D(x, y, z - 1),
  )

  def manhattanDistance(other: Coords3D): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)

  def +(other: Coords3D): Coords3D = Coords3D(
    x + other.x,
    y + other.y,
    z + other.z,
  )

  def -(other: Coords3D): Coords3D = Coords3D(
    x - other.x,
    y - other.y,
    z - other.z,
  )
}

object Coords3D {
  def parse(s: String): Coords3D =
    s match {
      case s"$x,$y,$z" => Coords3D(x.trim.toInt, y.trim.toInt, z.trim.toInt)
      case _           => sys.error(s"Failed to parse Coords3D '$s'")
    }

  def boundingBoxInclusive(points: Iterable[Coords3D]): Area3D = {
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    val minZ = points.map(_.z).min
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max
    val maxZ = points.map(_.z).max
    Area3D(Coords3D(minX, minY, minZ), Coords3D(maxX, maxY, maxZ))
  }
}
