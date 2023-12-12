package jurisk.adventofcode.y2023.pipe

import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Direction2D.E
import jurisk.geometry.Direction2D.N
import jurisk.geometry.Direction2D.S
import jurisk.geometry.Direction2D.W
import jurisk.geometry.Field2D

sealed trait Pipe {
  def symbol: Char
  def connections: Set[CardinalDirection2D]
}

case object Pipe {
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
