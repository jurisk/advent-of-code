package jurisk.geometry

final case class Area3D(min: Coords3D, max: Coords3D) {
  def contains(n: Coords3D): Boolean =
    n.x >= min.x && n.x <= max.x && n.y >= min.y && n.y <= max.y && n.z >= min.z && n.z <= max.z

  def points: Seq[Coords3D] =
    (min.x to max.x) flatMap { x =>
      (min.y to max.y) flatMap { y =>
        (min.z to max.z) map { z =>
          Coords3D(x, y, z)
        }
      }
    }

  def expandInEachDirectionBy(n: Int): Area3D = Area3D(
    min - Coords3D(n, n, n),
    max + Coords3D(n, n, n),
  )
}

object Area3D {
  def boundingBoxInclusive(points: Iterable[Coords3D]): Area3D =
    Coords3D.boundingBoxInclusive(points)
}
