package jurisk.geometry

final case class Area2D(min: Coords2D, max: Coords2D) {
  def onInsideEdge(c: Coords2D): Boolean =
    (c.x == min.x) || (c.x == max.x) || (c.y == min.y) || (c.y == max.y)

  def points: List[Coords2D] = {
    (min.x to max.x) flatMap { x =>
      (min.y to max.y) map { y =>
        Coords2D.of(x, y)
      }
    }
  }.toList

  def shiftBy(c: Coords2D): Area2D =
    Area2D(min + c, max + c)

  def +(c: Coords2D): Area2D =
    shiftBy(c)

  def topLeft: Coords2D     = min
  def bottomRight: Coords2D = max

  def height: Int = (max.y - min.y) + 1
  def width: Int  = (max.x - min.x) + 1

  def left: Int   = min.x
  def right: Int  = max.x
  def top: Int    = min.y
  def bottom: Int = max.y

  def expandInEachDirectionBy(n: Int): Area2D = Area2D(
    min - Coords2D.of(n, n),
    max + Coords2D.of(n, n),
  )

  def contains(c: Coords2D): Boolean =
    c.x >= min.x && c.x <= max.x && c.y >= min.y && c.y <= max.y
}

object Area2D {
  def fromLeftTopWidthHeight(
    left: Int,
    top: Int,
    width: Int,
    height: Int,
  ): Area2D =
    Area2D(
      Coords2D.of(left, top),
      Coords2D.of(left + width - 1, top + height - 1),
    )

  def boundingBoxInclusive(coords: Seq[Coords2D]): Area2D =
    Coords2D.boundingBoxInclusive(coords)
}
