package jurisk.geometry

import jurisk.math.Enumerated
import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Ordering.Implicits.infixOrderingOps

final case class Area3D[N: Numeric](min: Coords3D[N], max: Coords3D[N]) {
  def contains(n: Coords3D[N]): Boolean =
    n.x >= min.x && n.x <= max.x && n.y >= min.y && n.y <= max.y && n.z >= min.z && n.z <= max.z

  def points(implicit enumerated: Enumerated[N]): Seq[Coords3D[N]] =
    (min.x to max.x) flatMap { x =>
      (min.y to max.y) flatMap { y =>
        (min.z to max.z) map { z =>
          Coords3D(x, y, z)
        }
      }
    }

  def expandInEachDirectionBy(n: N): Area3D[N] = Area3D(
    min - Coords3D(n, n, n),
    max + Coords3D(n, n, n),
  )

  def moveBy(c: Coords3D[N]): Area3D[N] =
    Area3D(
      min = min + c,
      max = max + c,
    )
}

object Area3D {
  def boundingBoxInclusive[N: Numeric](
    points: Iterable[Coords3D[N]]
  ): Area3D[N] =
    Coords3D.boundingBoxInclusive(points)
}
