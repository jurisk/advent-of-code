package jurisk.adventofcode.y2024

import cats.implicits.toFunctorOps
import jurisk.geometry.Direction2D.{CardinalDirection2D, parseCaretToOption}
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent15 {
  sealed trait Square extends Product with Serializable {
    def toChar: Char =
      this match {
        case Square.Empty => '.'
        case Square.Wall  => '#'
        case Square.Box   => 'O'
      }
  }
  object Square {
    final case object Empty extends Square
    final case object Wall  extends Square
    final case object Box   extends Square
  }

  final case class State(
    robot: Coords2D,
    field: Field2D[Square],
  ) {
    def print(legend: String): Unit = {
      val charField = field.map(_.toChar)
      val f         = charField.updatedAtUnsafe(robot, '@')
      println(legend)
      Field2D.printCharField(f)
    }

    private def calculateMovePackage(
      c: Coords2D,
      direction: CardinalDirection2D,
    ): Option[List[Coords2D]] = {
      val next = c + direction
      field.at(next) match {
        case Some(Square.Empty) => Some(Nil)
        case Some(Square.Wall)  => None
        case Some(Square.Box)   =>
          calculateMovePackage(next, direction) match {
            case Some(more) => Some(next :: more)
            case None       => None
          }
        case None               => sys.error("unexpected")
      }
    }

    private def moveRobot(next: Coords2D): State =
      State(next, field)

    private def moveSquare(c: Coords2D, dir: CardinalDirection2D): State = {
      val next      = c + dir
      val a         = field.at(c).get
      val b         = field.at(next).get
      val nextField = field
        .updatedAtUnsafe(c, b)
        .updatedAtUnsafe(next, a)
      State(robot, nextField)
    }

    def move(direction: CardinalDirection2D): State = {
      val next        = robot + direction
      val movePackage = calculateMovePackage(robot, direction)
      movePackage match {
        case None    =>
          if (field.at(next).contains(Square.Empty)) {
            moveRobot(next)
          } else {
            this
          }
        case Some(p) =>
//          println(p)
          p.reverse
            .foldLeft(this) { (state, c) =>
              state.moveSquare(c, direction)
            }
            .moveRobot(next)
      }
    }
  }
  type Input = (State, List[CardinalDirection2D])
  type N = Long

  def parse(input: String): Input = {
    val (a, b)     = input.splitPairByDoubleNewline
    val charField  = Field2D.parseCharField(a)
    val robot      = charField.findCoordsByValue('@').get
    val field      = charField.map {
      case '.' => Square.Empty
      case '#' => Square.Wall
      case '@' => Square.Empty
      case 'O' => Square.Box
    }
    val state      = State(robot, field)
    val dirs       = b.splitLines.mkString
    val directions = dirs.map(parseCaretToOption(_).get).toList
    (state, directions)
  }

  def part1(data: Input): N = {
    val (state, directions) = data
    println(s"state: $state, directions: $directions")
    state.print("Initial state:")
    val result              = directions.foldLeft(state) { (state, direction) =>
      val r = state.move(direction)
      r.print(s"Move $direction:")
      r
    }
    result.print("Final state:")
    result.field.allCoords.map { c =>
      if (result.field.at(c).contains(Square.Box)) {
        100 * c.y + c.x
      } else {
        0
      }
    }.sum
  }

  def part2(data: Input): N =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/15$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
