package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent16.Square.{
  Empty,
  HorizontalSplitter,
  MirrorLeanLeft,
  MirrorLeanRight,
  VerticalSplitter,
}
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Direction2D.{CardinalDirection2D, E, N, S, W}
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import mouse.all.booleanSyntaxMouse

object Advent16 {
  type Input = Field2D[Square]

  sealed trait Square extends Product with Serializable {
    def incomingToOutgoing(
      incoming: CardinalDirection2D
    ): List[CardinalDirection2D] = this match {
      case Square.Empty              =>
        incoming.invert :: Nil
      case Square.MirrorLeanRight    =>
        (incoming match {
          case N => W
          case E => S
          case S => E
          case W => N
        }) :: Nil
      case Square.MirrorLeanLeft     =>
        (incoming match {
          case N => E
          case E => N
          case S => W
          case W => S
        }) :: Nil
      case Square.HorizontalSplitter =>
        incoming match {
          case N | S => E :: W :: Nil
          case ew    => ew.invert :: Nil
        }
      case Square.VerticalSplitter   =>
        incoming match {
          case E | W => N :: S :: Nil
          case ns    => ns.invert :: Nil
        }
    }
  }

  object Square {
    case object Empty              extends Square
    case object MirrorLeanRight    extends Square
    case object MirrorLeanLeft     extends Square
    case object HorizontalSplitter extends Square
    case object VerticalSplitter   extends Square
  }

  def parse(input: String): Input =
    Field2D.parseFromBiMap(
      input,
      BiMap(
        '.' <-> Empty,
        '/' <-> MirrorLeanRight,
        '\\' <-> MirrorLeanLeft,
        '-' <-> HorizontalSplitter,
        '|' <-> VerticalSplitter,
      ),
    )

  final case class SquareBeams(
    incoming: Set[CardinalDirection2D],
    outgoing: Set[CardinalDirection2D],
  ) {
    def nonEmpty: Boolean = incoming.nonEmpty || outgoing.nonEmpty
  }

  object SquareBeams {
    def empty: SquareBeams = SquareBeams(Set.empty, Set.empty)
  }

  type State = Field2D[SquareBeams]

  private def runBeams(field: Input, state: State): State = {
    def newOutgoings(
      field: Input,
      state: State,
    ): Seq[(Coords2D, CardinalDirection2D)] = {
      var results = List.empty[(Coords2D, CardinalDirection2D)]

      state.mapByCoordsWithValues { case (c, v) =>
        val square      = field.atOrElse(c, Empty)
        val incoming    = v.incoming
        val oldOutgoing = v.outgoing
        val newOutgoing = for {
          incoming <- incoming
          result   <- square.incomingToOutgoing(incoming)
        } yield result
        val diff        = newOutgoing -- oldOutgoing
        diff foreach { d =>
          results = (c -> d) :: results
        }
      }

      results
    }

    val newOut = newOutgoings(field, state)
    val newIn  = newOut flatMap { case (c, dir) =>
      val neighbourCoords = c + dir
      field.isValidCoordinate(neighbourCoords).option {
        neighbourCoords -> dir.invert
      }
    }

    val newOutMap = newOut.groupMap(_._1)(_._2)
    val newInMap  = newIn.groupMap(_._1)(_._2)

    state mapByCoordsWithValues { case (c, v) =>
      val oldIncoming = v.incoming
      val newIncoming = newInMap.getOrElse(c, Seq.empty)

      val oldOutgoing = v.outgoing
      val newOutgoing = newOutMap.getOrElse(c, Seq.empty)

      SquareBeams(
        incoming = oldIncoming ++ newIncoming.toSet,
        outgoing = oldOutgoing ++ newOutgoing.toSet,
      )
    }
  }

  private def solve(
    field: Input,
    initialSquare: Coords2D,
    initialDirection: CardinalDirection2D,
  ): Long = {
    val beams         = field.mapByCoords(_ => SquareBeams.empty)
    val initial       = beams.updatedAtUnsafe(
      initialSquare,
      SquareBeams(Set(initialDirection), Set.empty),
    )
    val (endState, _) = Simulation.runUntilStableState(initial) {
      case (state, _) =>
        runBeams(field, state)
    }
    endState.count(_.nonEmpty)
  }

  def part1(field: Input): Long =
    solve(field, Coords2D.Zero, Direction2D.W)

  def part2(field: Input): Long = {
    val fromNorth = field.topRowCoords.map {
      solve(field, _, Direction2D.N)
    }

    val fromSouth = field.bottomRowCoords.map {
      solve(field, _, Direction2D.S)
    }

    val fromWest = field.leftColumnCoords.map {
      solve(field, _, Direction2D.W)
    }

    val fromEast = field.rightColumnCoords.map {
      solve(field, _, Direction2D.W)
    }

    List(fromNorth, fromSouth, fromWest, fromEast).flatten.max
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/16.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
