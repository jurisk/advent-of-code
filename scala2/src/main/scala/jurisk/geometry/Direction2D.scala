package jurisk.geometry

import jurisk.geometry.Rotation._
import cats.implicits._

sealed trait Direction2D {
  def diff: Coords2D
}

object Direction2D {
  sealed trait CardinalDirection2D extends Direction2D {
    def rotate(rotation: Rotation): CardinalDirection2D =
      (rotation, this) match {
        case (NoRotation, _) => this
        case (Left90, N)     => W
        case (Left90, E)     => N
        case (Left90, S)     => E
        case (Left90, W)     => S
        case (TurnAround, N) => S
        case (TurnAround, E) => W
        case (TurnAround, S) => N
        case (TurnAround, W) => E
        case (Right90, N)    => E
        case (Right90, E)    => S
        case (Right90, S)    => W
        case (Right90, W)    => N
        case _               =>
          sys.error(s"rotate does not support rotating $this by $rotation")
      }
  }

  sealed trait DiagonalDirection2D extends Direction2D

  val CardinalDirections: List[CardinalDirection2D] = N :: S :: W :: E :: Nil
  val DiagonalDirections: List[DiagonalDirection2D] =
    NW :: NE :: SW :: SE :: Nil
  val AllDirections: List[Direction2D]              =
    CardinalDirections ::: DiagonalDirections

  case object N extends CardinalDirection2D {
    val diff: Coords2D = Coords2D.of(0, -1)
  }

  case object NE extends DiagonalDirection2D {
    val diff: Coords2D = N.diff + E.diff
  }

  case object NW extends DiagonalDirection2D {
    val diff: Coords2D = N.diff + W.diff
  }

  case object S extends CardinalDirection2D {
    val diff: Coords2D = Coords2D.of(0, +1)
  }

  case object SE extends DiagonalDirection2D {
    val diff: Coords2D = S.diff + E.diff
  }

  case object SW extends DiagonalDirection2D {
    val diff: Coords2D = S.diff + W.diff
  }

  case object W extends CardinalDirection2D {
    val diff: Coords2D = Coords2D.of(-1, 0)
  }

  case object E extends CardinalDirection2D {
    val diff: Coords2D = Coords2D.of(1, 0)
  }

  def parseUDLR(s: String): CardinalDirection2D = s match {
    case "U" => N
    case "D" => S
    case "L" => W
    case "R" => E
    case _   => sys.error(s"Unrecognized cardinal direction: $s")
  }

  def parseCaretToOption(ch: Char): Option[CardinalDirection2D] = ch match {
    case '>' => Direction2D.E.some
    case '^' => Direction2D.N.some
    case '<' => Direction2D.W.some
    case 'v' => Direction2D.S.some
    case _   => none
  }
}
