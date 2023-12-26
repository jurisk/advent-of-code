package jurisk.geometry

import jurisk.math.Enumerated
import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

// Inclusive
final case class Area2D[N: Numeric](
  min: Coordinates2D[N],
  max: Coordinates2D[N],
) {
  private val One = implicitly[Numeric[N]].one

  def onInsideEdge(c: Coordinates2D[N]): Boolean =
    (c.x == min.x) || (c.x == max.x) || (c.y == min.y) || (c.y == max.y)

  def points(implicit enumerable: Enumerated[N]): Seq[Coordinates2D[N]] =
    (min.x to max.x) flatMap { x =>
      (min.y to max.y) map { y =>
        Coordinates2D.of[N](x, y)
      }
    }

  def shiftBy(c: Coordinates2D[N]): Area2D[N] =
    Area2D[N](min + c, max + c)

  def +(c: Coordinates2D[N]): Area2D[N] =
    shiftBy(c)

  def topLeft: Coordinates2D[N]     = min
  def bottomRight: Coordinates2D[N] = max

  def height: N = (max.y - min.y) + One
  def width: N  = (max.x - min.x) + One

  def left: N   = min.x
  def right: N  = max.x
  def top: N    = min.y
  def bottom: N = max.y

  def expandInEachDirectionBy(n: N): Area2D[N] = Area2D[N](
    min - Coordinates2D.of[N](n, n),
    max + Coordinates2D.of[N](n, n),
  )

  def contains(c: Coordinates2D[N]): Boolean =
    c.x >= min.x && c.x <= max.x && c.y >= min.y && c.y <= max.y
}

object Area2D {
  def fromLeftTopWidthHeight[N: Numeric](
    left: N,
    top: N,
    width: N,
    height: N,
  ): Area2D[N] = {
    val One = implicitly[Numeric[N]].one

    Area2D(
      Coordinates2D.of(left, top),
      Coordinates2D.of(left + width - One, top + height - One),
    )
  }

  def boundingBoxInclusive[N: Numeric](
    coords: Seq[Coordinates2D[N]]
  ): Area2D[N] =
    Coordinates2D.boundingBoxInclusive(coords)
}
