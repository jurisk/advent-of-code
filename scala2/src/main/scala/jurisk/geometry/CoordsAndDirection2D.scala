package jurisk.geometry

import jurisk.geometry.Direction2D._

final case class CoordsAndDirection2D(
  coords: Coords2D,
  direction: CardinalDirection2D,
) {
  def nextStraight: CoordsAndDirection2D =
    CoordsAndDirection2D(coords + direction, direction)

  def moveInDirection(newDirection: CardinalDirection2D): CoordsAndDirection2D =
    CoordsAndDirection2D(
      coords + newDirection,
      newDirection,
    )
}
