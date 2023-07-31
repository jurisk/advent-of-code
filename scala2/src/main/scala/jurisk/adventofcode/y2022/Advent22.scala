package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.adventofcode.y2022.Advent22.Square.Open
import jurisk.adventofcode.y2022.Advent22.Square.Outside
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D._
import jurisk.geometry.Field2D
import jurisk.geometry.Rotation
import jurisk.geometry.Rotation.TurnAround
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent22 {
  type Parsed = (Field2D[Square], List[Command])

  sealed trait Command
  object Command {
    final case class Walk(distance: Int)        extends Command
    final case class Rotate(rotation: Rotation) extends Command
  }

  sealed trait Square
  object Square {
    case object Outside extends Square
    case object Wall    extends Square
    case object Open    extends Square

    def parse(ch: Char): Square =
      ch match {
        case ' ' => Outside
        case '#' => Wall
        case '.' => Open
      }
  }

  @tailrec
  private def parseCommands(
    input: String,
    acc: Vector[Command],
  ): Vector[Command] =
    if (input.isEmpty) {
      acc
    } else {
      val digits = input.takeWhile(_.isDigit)
      if (digits.isEmpty) {
        val rotation = input.head match {
          case 'L' => Rotation.Left90
          case 'R' => Rotation.Right90
        }
        parseCommands(input.tail, acc :+ Command.Rotate(rotation))
      } else {
        parseCommands(
          input.drop(digits.length),
          acc :+ Command.Walk(digits.toInt),
        )
      }
    }

  def parse(data: String): Parsed = {
    val Array(fieldS, commandsS) = data.split("\n\n")

    val field    = Field2D.parseFromString(fieldS, Square.parse)
    val commands = parseCommands(commandsS, Vector.empty).toList
    (field, commands)
  }

  final case class State(
    field: Field2D[Square],
    position: Coords2D,
    direction: CardinalDirection2D,
    logicType: LogicType,
  ) {
    private def isValid(c: Coords2D): Boolean = field.at(c) match {
      case Some(Outside) => false
      case None          => false
      case _             => true
    }

    def debugPrint(): Unit = {
      val charField = field.map {
        case Square.Outside => ' '
        case Square.Wall    => '#'
        case Square.Open    => '·'
      }

      val caret = direction match {
        case Direction2D.N => '^'
        case Direction2D.S => 'v'
        case Direction2D.W => '<'
        case Direction2D.E => '>'
      }

      val withCharacter = if (charField.isValidCoordinate(position)) {
        charField.updatedAtUnsafe(position, caret)
      } else {
        println(s"Unexpectedly position is out of field $position")
        charField
      }

      println(Field2D.toDebugRepresentation(withCharacter))
      println()
    }

    private def nextWithWrapAround1: (Coords2D, CardinalDirection2D) = {
      def firstSuitable(coords: List[Coords2D]): Coords2D =
        coords.find(isValid).get

      val raw   = position + direction
      val valid = isValid(raw)

      val newPosition = if (valid) {
        raw
      } else {
        direction match {
          case Direction2D.N =>
            firstSuitable(field.coordsForColumn(position.x).reverse)
          case Direction2D.S => firstSuitable(field.coordsForColumn(position.x))
          case Direction2D.W =>
            firstSuitable(field.coordsForRow(position.y).reverse)
          case Direction2D.E => firstSuitable(field.coordsForRow(position.y))
        }
      }

      (newPosition, direction)
    }

    /* These transitions only support the following cube layout:
                    ┌─────┬─────┐
                    │     │     │
                    │ 1,0 │ 2,1 │
                    │     │     │
                    ├─────┼─────┘
                    │     │
                    │ 1,1 │
                    │     │
              ┌─────┼─────┤
              │     │     │
              │ 0,2 │ 1,2 │
              │     │     │
              ├─────┼─────┘
              │     │
              │ 0,3 │
              │     │
              └─────┘
     */
    private def nextWithWrapAround2: (Coords2D, CardinalDirection2D) = {
      val raw = position + direction
      if (isValid(raw)) {
        (raw, direction)
      } else {
        val sideLength: Int =
          Set(field.width / 3, field.height / 4).singleElementUnsafe

        val currentVerticalLane   = position.x / sideLength
        val xOffset               = position.x % sideLength
        val currentHorizontalLane = position.y / sideLength
        val yOffset               = position.y % sideLength

        (currentVerticalLane, currentHorizontalLane, direction) match {
          case (1, 0, W) => (Coords2D.of(0, 3 * sideLength - yOffset - 1), E)
          case (1, 1, W) => (Coords2D.of(yOffset, 2 * sideLength), S)
          case (0, 2, N) => (Coords2D.of(sideLength, sideLength + xOffset), E)
          case (0, 2, W) =>
            (Coords2D.of(sideLength, sideLength - yOffset - 1), E)
          case (0, 3, W) => (Coords2D.of(sideLength + yOffset, 0), S)
          case (0, 3, S) => (Coords2D.of(2 * sideLength + xOffset, 0), S)
          case (0, 3, E) =>
            (Coords2D.of(sideLength + yOffset, 3 * sideLength - 1), N)
          case (1, 2, S) =>
            (Coords2D.of(sideLength - 1, sideLength * 3 + xOffset), W)
          case (1, 2, E) =>
            (Coords2D.of(sideLength * 3 - 1, sideLength - yOffset - 1), W)
          case (1, 1, E) =>
            (Coords2D.of(2 * sideLength + yOffset, sideLength - 1), N)
          case (2, 0, S) =>
            (Coords2D.of(2 * sideLength - 1, sideLength + xOffset), W)
          case (2, 0, E) =>
            (Coords2D.of(2 * sideLength - 1, 3 * sideLength - yOffset - 1), W)
          case (2, 0, N) => (Coords2D.of(xOffset, 4 * sideLength - 1), N)
          case (1, 0, N) => (Coords2D.of(0, 3 * sideLength + xOffset), E)
          case _         =>
            s"Unexpected lanes $currentVerticalLane $currentVerticalLane $direction for $position".fail
        }
      }
    }

    private def nextWithWrapAround: (Coords2D, CardinalDirection2D) =
      logicType match {
        case LogicType.Part1 => nextWithWrapAround1
        case LogicType.Part2 => nextWithWrapAround2
      }

    private def walkForwardOne: State = {
      val (nextCoords, nextDirection) = nextWithWrapAround

      // Sanity check
      val sanityCheck = copy(
        position = nextCoords,
        direction = nextDirection.rotate(TurnAround),
      ).nextWithWrapAround
      sanityCheck._1 shouldEqual position
      sanityCheck._2.rotate(TurnAround) shouldEqual direction

      field(nextCoords) match {
        case Square.Outside => sys.error("Should not happen")
        case Square.Wall    => this
        case Square.Open    =>
          copy(position = nextCoords, direction = nextDirection)
      }
    }

    def next(command: Command): State =
      command match {
        case Command.Walk(distance)   =>
          (0 until distance).foldLeft(this) { case (acc, _) =>
            acc.walkForwardOne
          }
        case Command.Rotate(rotation) =>
          copy(direction = direction.rotate(rotation))
      }
  }

  sealed trait LogicType
  object LogicType {
    case object Part1 extends LogicType
    case object Part2 extends LogicType
  }

  def solve(data: Parsed, logicType: LogicType): Int = {
    val (field, commands) = data

    val start = field
      .filterCoordsByValue(_ == Open)
      .filter(_.y == 0)
      .minBy(_.x)

    val startState = State(
      field,
      start,
      Direction2D.E,
      logicType,
    )

    val result = commands.foldLeft(startState) { case (state, command) =>
      state.next(command)
    }

    val rx = result.position.x + 1
    val ry = result.position.y + 1
    val rd = result.direction match {
      case Direction2D.N => 3
      case Direction2D.S => 1
      case Direction2D.W => 2
      case Direction2D.E => 0
    }

    1000 * ry + 4 * rx + rd
  }

  def part1(data: Parsed): Int =
    solve(data, LogicType.Part1)

  def part2(data: Parsed): Int =
    solve(data, LogicType.Part2)

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/22-test.txt")
    val realData = readFileText("2022/22.txt")

    val test = parse(testData)

    val real = parse(realData)

    part1(test) shouldEqual 6032
    part1(real) shouldEqual 164014

    // Our logic for Part 2 only supports the real data set, as the test data set has a different format
    part2(real) shouldEqual 47525
  }
}
