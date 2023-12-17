package jurisk.geometry

import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Rotation._
import jurisk.utils.Parsing.StringOps

sealed trait Direction2D extends Product with Serializable {
  def diff: Coords2D
}

object Direction2D {
  sealed trait CardinalDirection2D extends Direction2D {
    def isVertical: Boolean
    def isHorizontal: Boolean = !isVertical

    def toCaret: Char = caretMapping.rightToLeftUnsafe(this)

    def invert: CardinalDirection2D = rotate(Rotation.TurnAround)

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
        case _               => s"rotate does not support rotating $this by $rotation".fail
      }

    def asString: String          = neswMapping.rightToLeftUnsafe(this)
    override def toString: String = asString
  }

  sealed trait DiagonalDirection2D extends Direction2D {
    def asString: String          = neswMapping.rightToLeftUnsafe(this)
    override def toString: String = asString
  }

  val CardinalDirections: List[CardinalDirection2D] = N :: S :: W :: E :: Nil
  val DiagonalDirections: List[DiagonalDirection2D] =
    NW :: NE :: SW :: SE :: Nil
  val AllDirections: List[Direction2D]              =
    CardinalDirections ::: DiagonalDirections

  case object N extends CardinalDirection2D {
    val diff: Coords2D               = Coords2D.of(0, -1)
    override def isVertical: Boolean = true
  }

  case object NE extends DiagonalDirection2D {
    val diff: Coords2D = N.diff + E.diff
  }

  case object NW extends DiagonalDirection2D {
    val diff: Coords2D = N.diff + W.diff
  }

  case object S extends CardinalDirection2D {
    val diff: Coords2D               = Coords2D.of(0, +1)
    override def isVertical: Boolean = true
  }

  case object SE extends DiagonalDirection2D {
    val diff: Coords2D = S.diff + E.diff
  }

  case object SW extends DiagonalDirection2D {
    val diff: Coords2D = S.diff + W.diff
  }

  case object W extends CardinalDirection2D {
    val diff: Coords2D               = Coords2D.of(-1, 0)
    override def isVertical: Boolean = false
  }

  case object E extends CardinalDirection2D {
    val diff: Coords2D               = Coords2D.of(1, 0)
    override def isVertical: Boolean = false
  }

  private val neswMapping: BiMap[String, Direction2D] = BiMap(
    "N" <-> N,
    "NE" <-> NE,
    "NW" <-> NW,
    "SE" <-> SE,
    "SW" <-> SW,
    "S" <-> S,
    "W" <-> W,
    "E" <-> E,
  )

  private val udlrMapping: BiMap[Char, CardinalDirection2D] = BiMap(
    'U' <-> N,
    'D' <-> S,
    'L' <-> W,
    'R' <-> E,
  )

  def parseUDLR(s: String): CardinalDirection2D = s.toList match {
    case ch :: Nil => udlrMapping.leftToRightUnsafe(ch)
    case _         => s.failedToParse("CardinalDirection2D")
  }

  private val caretMapping: BiMap[Char, CardinalDirection2D] = BiMap(
    '^' <-> N,
    'v' <-> S,
    '<' <-> W,
    '>' <-> E,
  )

  def parseCaretToOption(ch: Char): Option[CardinalDirection2D] =
    caretMapping.leftToRight(ch)
}
