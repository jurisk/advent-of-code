package jurisk.adventofcode.y2022

import jurisk.adventofcode.y2022.Advent22.Square.{Open, Outside}
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent22 {
  type Parsed = (Field2D[Square], List[Command])

  sealed trait Command
  object Command {
    final case class Walk(distance: Int) extends Command
    final case class Rotate(rotation: Rotation) extends Command
  }

  sealed trait Square
  object Square {
    final case object Outside extends Square
    final case object Wall extends Square
    final case object Open extends Square

    def parse(ch: Char): Square =
      ch match {
        case ' '            => Outside
        case '#'            => Wall
        case '.'            => Open
      }
  }

  @tailrec
  private def parseCommands(input: String, acc: Vector[Command]): Vector[Command] = {
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
        parseCommands(input.drop(digits.length), acc :+ Command.Walk(digits.toInt))
      }
    }
  }

  def parse(data: String): Parsed = {
    val Array(fieldS, commandsS) = data.split("\n\n")

    val field = Field2D.parseFromString(fieldS, Square.parse)
    val commands = parseCommands(commandsS, Vector.empty).toList
    (field, commands)
  }

  final case class State(field: Field2D[Square], position: Coords2D, direction: CardinalDirection2D) {
    def firstSuitable(coords: List[Coords2D]): Coords2D = {
      coords.find(isValid).get
    }

    def isValid(c: Coords2D): Boolean = field.at(c) match {
      case Some(Outside) => false
      case None => false
      case _ => true
    }

    def smartNext: Coords2D = {
      val raw = position + direction.diff
      val valid = isValid(raw)

      if (valid) {
        raw
      } else {
        direction match {
          case Direction2D.N => firstSuitable(field.coordsForColumn(position.x).reverse)
          case Direction2D.S => firstSuitable(field.coordsForColumn(position.x))
          case Direction2D.W => firstSuitable(field.coordsForRow(position.y).reverse)
          case Direction2D.E => firstSuitable(field.coordsForRow(position.y))
        }
      }
    }

    def walkForwardOne: State = {
      val nextCoords = smartNext
      field(nextCoords) match {
        case Square.Outside => sys.error("Should not happen")
        case Square.Wall => this
        case Square.Open => copy(position = nextCoords)
      }
    }

    def next(command: Command): State = {
      command match {
        case Command.Walk(distance) =>
          (0 until distance).foldLeft(this) { case (acc, _) =>
            acc.walkForwardOne
          }
        case Command.Rotate(rotation) => copy(direction = direction.rotate(rotation))
      }
    }
  }

  def part1(data: Parsed): Int = {
    val (field, commands) = data

    val start = field.filterCoordsByValue(_ == Open).filter(_.y.value == 0).minBy(_.x.value)
    val startState = State(
      field,
      start,
      Direction2D.E,
    )

    val result = commands.foldLeft(startState) { case (state, command) =>
      state.next(command)
    }

    val rx = result.position.x.value + 1
    val ry = result.position.y.value + 1
    val rd = result.direction match {
      case Direction2D.N => 3
      case Direction2D.S => 1
      case Direction2D.W => 2
      case Direction2D.E => 0
    }

    1000 * ry + 4 * rx + rd
  }

  def part2(data: Parsed): Int =
    ???


  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/22-test.txt")
    val realData = readFileText("2022/22.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 6032
    part1(real) shouldEqual 164014

    part2(test) shouldEqual 5031
    part2(real) shouldEqual "asdf"
  }
}
