package jurisk.adventofcode.y2024

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2024.Advent15.Square.Wall
import jurisk.geometry.Direction2D.{
  CardinalDirection2D,
  E,
  N,
  S,
  W,
  parseCaretToOption,
}
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent15 {
  sealed trait Square extends Product with Serializable {
    def toChar: Char =
      this match {
        case Square.Empty    => '.'
        case Square.Wall     => '#'
        case Square.SmallBox => 'O'
        case Square.LeftBox  => '['
        case Square.RightBox => ']'
      }
  }
  object Square {
    final case object Empty    extends Square
    final case object Wall     extends Square
    final case object SmallBox extends Square
    final case object LeftBox  extends Square
    final case object RightBox extends Square

    def parse(c: Char): Square =
      c match {
        case '.' | '@' => Empty
        case '#'       => Wall
        case 'O'       => SmallBox
        case '['       => LeftBox
        case ']'       => RightBox
        case _         => s"unexpected char: $c".fail
      }
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

    private def moveMany(
      coords: List[Coords2D],
      dir: CardinalDirection2D,
    ): State = {
      val valid  = coords.forall { c =>
        val n = c + dir
        field.at(n).contains(Square.Empty) || coords.contains(n)
      }
      assert(valid)
      var result = field
      coords.foreach { c =>
        result = result.updatedAtUnsafe(c, Square.Empty)
      }
      coords.foreach { c =>
        val n = c + dir
        result = result.updatedAtUnsafe(n, field.at(c).get)
      }
      State(robot, result)
    }

    private def calculateMovePackage(
      c: Coords2D,
      direction: CardinalDirection2D,
    ): Option[List[Coords2D]] = {
      val next = c + direction
      field.at(next) match {
        case Some(Square.Empty) => Some(Nil)

        case Some(Square.Wall) => None

        case Some(Square.LeftBox) if Set(N, S).contains(direction) =>
          val otherC = next + E
          (
            calculateMovePackage(next, direction),
            calculateMovePackage(otherC, direction),
          ) match {
            case (Some(a), Some(b)) => Some(next :: otherC :: a ::: b)
            case _                  => None
          }

        case Some(Square.RightBox) if Set(N, S).contains(direction) =>
          val otherC = next + W
          (
            calculateMovePackage(next, direction),
            calculateMovePackage(otherC, direction),
          ) match {
            case (Some(a), Some(b)) => Some(next :: otherC :: a ::: b)
            case _                  => None
          }

        case Some(Square.RightBox) | Some(Square.LeftBox) | Some(
              Square.SmallBox
            ) =>
          calculateMovePackage(next, direction) match {
            case Some(more) => Some(next :: more)
            case None       => None
          }

        case None => sys.error("unexpected")
      }
    }

    private def moveRobot(next: Coords2D): State =
      if (field.at(next).contains(Square.Empty)) {
        State(next, field)
      } else {
        sys.error("unexpected")
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
          moveMany(p, direction)
            .moveRobot(next)
      }
    }
  }

  type Input = (State, List[CardinalDirection2D])
  type N     = Long

  def parse(input: String): Input = {
    val (a, b)     = input.splitPairByDoubleNewline
    val charField  = Field2D.parseCharField(a)
    val robot      = charField.findCoordsByValue('@').get
    val field      = charField.map(Square.parse)
    val state      = State(robot, field)
    val dirs       = b.splitLines.mkString
    val directions = dirs.map(parseCaretToOption(_).get).toList
    (state, directions)
  }

  def part1(data: Input): N = {
    val (state, directions) = data
    state.print("Initial state:")
    val result              = directions.foldLeft(state) { (state, direction) =>
      state.move(direction)
    }
    result.print("Final state:")
    result.field.allCoordsAndValues.map { case (c, v) =>
      if (Set(Square.LeftBox, Square.SmallBox).contains(v)) {
        100 * c.y + c.x
      } else {
        0
      }
    }.sum
  }

  def part2(data: Input): N = {
    val (incomingState, directions) = data
    var field: Field2D[Square]      =
      Field2D.ofSize(
        incomingState.field.width * 2,
        incomingState.field.height,
        Square.Empty,
      )
    incomingState.field.allCoordsAndValues.foreach { case (c, v) =>
      val c1 = Coords2D(c.x * 2, c.y)
      val c2 = Coords2D(c.x * 2 + 1, c.y)
      val v1 = v match {
        case Square.Empty    => Square.Empty
        case Square.Wall     => Square.Wall
        case Square.SmallBox => Square.LeftBox
        case _               => s"Unexpected value: $v".fail
      }
      val v2 = v match {
        case Square.Empty    => Square.Empty
        case Square.Wall     => Square.Wall
        case Square.SmallBox => Square.RightBox
        case _               => s"Unexpected value: $v".fail
      }
      field = field.updatedAtUnsafe(c1, v1)
      field = field.updatedAtUnsafe(c2, v2)
    }
    val newRobot                    = Coords2D(incomingState.robot.x * 2, incomingState.robot.y)
    val state                       = State(newRobot, field)

    part1((state, directions))
  }

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
