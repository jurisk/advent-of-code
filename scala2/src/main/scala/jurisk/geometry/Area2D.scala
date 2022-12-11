package jurisk.geometry

final case class Area2D(left: X, top: Y, width: Int, height: Int) {
  def pointSet: Set[Coords2D] = {
    (0 until width) flatMap { w =>
      (0 until height) map { h => Coords2D(left + w, top + h) }
    }
  }.toSet

  def topLeft: Coords2D     = Coords2D(left, top)
  def bottomRight: Coords2D = Coords2D(left + width, top + height)
}
