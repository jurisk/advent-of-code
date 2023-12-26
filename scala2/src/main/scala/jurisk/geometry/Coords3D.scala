package jurisk.geometry

import jurisk.utils.Parsing.StringOps

import scala.math.Numeric.Implicits.infixNumericOps

final case class Coords3D[N: Numeric](x: N, y: N, z: N) {
  private val One = implicitly[Numeric[N]].one

  def adjacent6: List[Coords3D[N]] = List(
    Coords3D(x + One, y, z),
    Coords3D(x - One, y, z),
    Coords3D(x, y + One, z),
    Coords3D(x, y - One, z),
    Coords3D(x, y, z + One),
    Coords3D(x, y, z - One),
  )

  def manhattanDistance(other: Coords3D[N]): N =
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  def +(other: Coords3D[N]): Coords3D[N] = Coords3D(
    x + other.x,
    y + other.y,
    z + other.z,
  )

  def -(other: Coords3D[N]): Coords3D[N] = Coords3D(
    x - other.x,
    y - other.y,
    z - other.z,
  )
}

object Coords3D {
  def Zero[N: Numeric]: Coords3D[N] = {
    val Z = implicitly[Numeric[N]].zero
    Coords3D(Z, Z, Z)
  }

  def parse[N: Numeric](s: String): Coords3D[N] = {
    val numeric = implicitly[Numeric[N]]
    s match {
      case s"$xs,$ys,$zs" =>
        val x: N =
          numeric.parseString(xs.trim) getOrElse s"Failed to parse $xs".fail
        val y: N =
          numeric.parseString(xs.trim) getOrElse s"Failed to parse $ys".fail
        val z: N =
          numeric.parseString(zs.trim) getOrElse s"Failed to parse $zs".fail

        Coords3D[N](x, y, z)
      case _              => s.failedToParse("Coords3D")
    }
  }

  def boundingBoxInclusive[N: Numeric](
    points: Iterable[Coords3D[N]]
  ): Area3D[N] = {
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    val minZ = points.map(_.z).min
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max
    val maxZ = points.map(_.z).max
    Area3D(Coords3D(minX, minY, minZ), Coords3D(maxX, maxY, maxZ))
  }
}
