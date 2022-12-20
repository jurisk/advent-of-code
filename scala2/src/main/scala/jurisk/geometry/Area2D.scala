package jurisk.geometry

final case class Area2D(min: Coords2D, max: Coords2D) {
  def points: List[Coords2D] = {
    (min.x.value to max.x.value) flatMap { x =>
      (min.y.value to max.y.value) map { y =>
        Coords2D.of(x, y)
      }
    }
  }.toList

  def topLeft: Coords2D     = min
  def bottomRight: Coords2D = max

  def height: Int = (max.y - min.y).value + 1
  def width: Int  = (max.x - min.x).value + 1

  def left: X = min.x
  def top: Y  = min.y

  def expandInEachDirectionBy(n: Int): Area2D = Area2D(
    min - Coords2D.of(n, n),
    max + Coords2D.of(n, n),
  )
}

object Area2D {
  def fromLeftTopWidthHeight(left: X, top: Y, width: Int, height: Int): Area2D =
    Area2D(Coords2D(left, top), Coords2D(left + width - 1, top + height - 1))
}
