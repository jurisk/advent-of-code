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

  final case class State(
    incomingQueue: Set[(Coords2D, CardinalDirection2D)],
    outgoingQueue: Set[(Coords2D, CardinalDirection2D)],
    incomingEdgesProcessed: Set[(Coords2D, CardinalDirection2D)],
  ) {
    def next(field: Input): State = {
      val newIncomingQueue = outgoingQueue.flatMap { case (c, dir) =>
        val neighbourCoords = c + dir
        field.isValidCoordinate(neighbourCoords) option {
          neighbourCoords -> dir.invert
        }
      } -- incomingEdgesProcessed

      val newOutgoingQueue = for {
        (c, dir) <- incomingQueue
        values    = field.at(c).toSet
        v        <- values
        outgoing <- v.incomingToOutgoing(dir)
      } yield c -> outgoing

      State(
        newIncomingQueue,
        newOutgoingQueue,
        incomingEdgesProcessed ++ newIncomingQueue,
      )
    }
  }

  object State {
    def fromInitial(
      initialSquare: Coords2D,
      initialDirection: CardinalDirection2D,
    ): State = {
      val incomingQueue = Set(initialSquare -> initialDirection)
      State(
        incomingQueue = incomingQueue,
        outgoingQueue = Set.empty,
        incomingEdgesProcessed = incomingQueue,
      )
    }
  }

  private def solve(
    field: Input,
    initialSquare: Coords2D,
    initialDirection: CardinalDirection2D,
  ): Long = {
    val initial = State.fromInitial(initialSquare, initialDirection)

    val (endState, _) = Simulation.runUntilStableState(initial) {
      case (state, _) =>
        state.next(field)
    }

    endState.incomingEdgesProcessed.map { case (c, _) => c }.size
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
