package jurisk.adventofcode.y2023.pipe

import jurisk.geometry.Coords2D
import jurisk.geometry.CoordsAndDirection2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Direction2D.E
import jurisk.geometry.Direction2D.N
import jurisk.geometry.Direction2D.NE
import jurisk.geometry.Direction2D.NW
import jurisk.geometry.Direction2D.S
import jurisk.geometry.Direction2D.SE
import jurisk.geometry.Direction2D.SW
import jurisk.geometry.Direction2D.W
import jurisk.geometry.Field2D
import jurisk.geometry.Rotation
import jurisk.utils.CollectionOps.IterableOps

sealed trait Pipe extends Product with Serializable {
  def symbol: Char
  def connections: Set[CardinalDirection2D]
}

object Pipe {
  // Next coordinate & direction if we walk along the track
  def nextOnTrack(
    current: CoordsAndDirection2D,
    field: Field2D[Pipe],
  ): CoordsAndDirection2D = {
    val next       = current.nextStraight
    val nextSquare = field.atOrElse(next.coords, Empty)

    val nextDirection = nextSquare.connections
      .filterNot(_ == current.direction.invert)
      .singleElementUnsafe

    next.copy(direction = nextDirection)
  }

  // Coordinates to the right of these `coords`, if facing in `direction`
  def coordsToTheRight(
    current: CoordsAndDirection2D,
    field: Field2D[Pipe],
  ): List[Coords2D] = {
    val direction                = current.direction
    val diffs: List[Direction2D] = field.atOrElse(current.coords, Empty) match {
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

    diffs.map(x => current.coords + x)
  }

  def connectedNeighbours(
    field: Field2D[Pipe],
    coords: Coords2D,
  ): List[Coords2D] =
    field
      .atOrElse(coords, Empty)
      .connections
      .filter { direction =>
        field
          .atOrElse(coords + direction, Empty)
          .connections
          .contains(direction.invert)
      }
      .map { direction =>
        coords + direction
      }
      .toList

  val NonEmpty: List[Pipe] = N_S :: E_W :: N_E :: N_W :: S_W :: S_E :: Nil
  val All: List[Pipe]      = Empty :: NonEmpty

  case object Empty extends Pipe {
    override def symbol: Char                          = '░'
    override def connections: Set[CardinalDirection2D] = Set.empty
  }

  case object N_S extends Pipe {
    override def symbol: Char                          = '┃'
    override def connections: Set[CardinalDirection2D] = Set(N, S)
  }

  case object E_W extends Pipe {
    override def symbol: Char                          = '━'
    override def connections: Set[CardinalDirection2D] = Set(E, W)
  }

  case object N_E extends Pipe {
    override def symbol: Char                          = '┗'
    override def connections: Set[CardinalDirection2D] = Set(N, E)
  }

  case object N_W extends Pipe {
    override def symbol: Char                          = '┛'
    override def connections: Set[CardinalDirection2D] = Set(N, W)
  }

  case object S_W extends Pipe {
    override def symbol: Char                          = '┓'
    override def connections: Set[CardinalDirection2D] = Set(S, W)
  }

  case object S_E extends Pipe {
    override def symbol: Char                          = '┏'
    override def connections: Set[CardinalDirection2D] = Set(S, E)
  }
}
