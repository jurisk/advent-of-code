package jurisk.geometry

case class Area3D(min: Coords3D, max: Coords3D) {
  def contains(n: Coords3D): Boolean =
    n.x >= min.x && n.x <= max.x && n.y >= min.y && n.y <= max.y && n.z >= min.z && n.z <= max.z

  def points: List[Coords3D] = {
    (min.x to max.x) flatMap { x =>
      (min.y to max.y) flatMap { y =>
        (min.z to max.z) map { z =>
          Coords3D(x, y, z)
        }
      }
    }
  }.toList
}
