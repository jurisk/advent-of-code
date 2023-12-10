package jurisk.adventofcode.y2023.pipe

import jurisk.adventofcode.y2023.pipe.Pipe.Empty
import jurisk.geometry.Direction2D._
import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps

final case class CoordsWithDirection(
  coords: Coords2D,
  direction: CardinalDirection2D,
) {
  // Next coordinate & direction if we walk along the track
  def nextOnTrack(field: Field2D[Pipe]): CoordsWithDirection = {
    val nextCoords = coords + direction
    val nextSquare = field.atOrElse(nextCoords, Empty)

    val nextDirection = nextSquare.connections
      .filterNot(_ == direction.invert)
      .singleElementUnsafe

    CoordsWithDirection(
      coords = nextCoords,
      direction = nextDirection,
    )
  }

  // Coordinates to the right of these `coords`, if facing in `direction`
  def coordsToTheRight(field: Field2D[Pipe]): List[Coords2D] = {
    val diffs: List[Direction2D] = field.atOrElse(coords, Empty) match {
      case Pipe.Empty => Nil

      case Pipe.N_S | Pipe.E_W =>
        direction.rotate(Rotation.Right90) :: Nil

      case Pipe.N_E =>
        direction match {
          case Direction2D.E => W :: SW :: S :: Nil
          case _             => Nil
        }
      case Pipe.N_W =>
        direction match {
          case Direction2D.N => S :: SE :: E :: Nil
          case _             => Nil
        }
      case Pipe.S_W =>
        direction match {
          case Direction2D.W => N :: NE :: E :: Nil
          case _             => Nil
        }
      case Pipe.S_E =>
        direction match {
          case Direction2D.S => N :: NW :: W :: Nil
          case _             => Nil
        }
    }

    diffs.map(x => coords + x)
  }
}
