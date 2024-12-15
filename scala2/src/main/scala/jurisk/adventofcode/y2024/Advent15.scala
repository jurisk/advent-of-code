package jurisk.adventofcode.y2024

import cats.implicits.toFunctorOps
import jurisk.geometry.Direction2D.{
  CardinalDirection2D,
  E,
  N,
  W,
  S,
  parseCaretToOption,
}
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent15 {
  sealed trait Square2 extends Product with Serializable {
    def toChar: Char =
      this match {
        case Square2.Empty    => '.'
        case Square2.Wall     => '#'
        case Square2.SmallBox => 'O'
        case Square2.LeftBox  => '['
        case Square2.RightBox => ']'
      }
  }
  object Square2 {
    final case object Empty    extends Square2
    final case object Wall     extends Square2
    final case object SmallBox extends Square2
    final case object LeftBox  extends Square2
    final case object RightBox extends Square2

    def parse(c: Char): Square2 =
      c match {
        case '.' | '@' => Empty
        case '#'       => Wall
        case 'O'       => SmallBox
        case '['       => LeftBox
        case ']'       => RightBox
        case _         => s"unexpected char: $c".fail
      }
  }

  final case class State2(
    robot: Coords2D,
    field: Field2D[Square2],
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
    ): State2 = {
      val valid  = coords.forall { c =>
        val n = c + dir
        field.at(n).contains(Square2.Empty) || coords.contains(n)
      }
      assert(valid)
      var result = field
      coords.foreach { c =>
        result = result.updatedAtUnsafe(c, Square2.Empty)
      }
      coords.foreach { c =>
        val n = c + dir
        result = result.updatedAtUnsafe(n, field.at(c).get)
      }
      State2(robot, result)
    }

    private def calculateMovePackage(
      c: Coords2D,
      direction: CardinalDirection2D,
    ): Option[List[Coords2D]] = {
      val next = c + direction
      field.at(next) match {
        case Some(Square2.Empty) => Some(Nil)

        case Some(Square2.Wall) => None

        case Some(Square2.LeftBox) if Set(N, S).contains(direction) =>
          val otherC = next + E
          (
            calculateMovePackage(next, direction),
            calculateMovePackage(otherC, direction),
          ) match {
            case (Some(a), Some(b)) => Some(next :: otherC :: a ::: b)
            case _                  => None
          }

        case Some(Square2.RightBox) if Set(N, S).contains(direction) =>
          val otherC = next + W
          (
            calculateMovePackage(next, direction),
            calculateMovePackage(otherC, direction),
          ) match {
            case (Some(a), Some(b)) => Some(next :: otherC :: a ::: b)
            case _                  => None
          }

        case Some(Square2.RightBox) | Some(Square2.LeftBox) | Some(
              Square2.SmallBox
            ) =>
          calculateMovePackage(next, direction) match {
            case Some(more) => Some(next :: more)
            case None       => None
          }

        case None => sys.error("unexpected")
      }
    }

    private def moveRobot(next: Coords2D): State2 =
      if (field.at(next).contains(Square2.Empty)) {
        State2(next, field)
      } else {
        sys.error("unexpected")
      }

    def move(direction: CardinalDirection2D): State2 = {
      val next        = robot + direction
      val movePackage = calculateMovePackage(robot, direction)
      movePackage match {
        case None    =>
          if (field.at(next).contains(Square2.Empty)) {
            moveRobot(next)
          } else {
            this
          }
        case Some(p) =>
          //          println(p)
          moveMany(p, direction)
            .moveRobot(next)
      }
    }
  }

  type Input = (State2, List[CardinalDirection2D])
  type N     = Long

  def parse(input: String): Input = {
    val (a, b)     = input.splitPairByDoubleNewline
    val charField  = Field2D.parseCharField(a)
    val robot      = charField.findCoordsByValue('@').get
    val field      = charField.map(Square2.parse)
    val state      = State2(robot, field)
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
//      r.print(s"Move $direction:")
      r
    }
    result.print("Final state:")
    result.field.allCoords.map { c =>
      if (result.field.at(c).contains(Square2.SmallBox)) {
        100 * c.y + c.x
      } else {
        0
      }
    }.sum
  }

  def part2(data: Input): N = {
    val (state1, directions)    = data
    var field: Field2D[Square2] =
      Field2D.ofSize(state1.field.width * 2, state1.field.height, Square2.Empty)
    state1.field.allCoords.foreach { c =>
      val v  = state1.field.at(c).get
      val c1 = Coords2D(c.x * 2, c.y)
      val c2 = Coords2D(c.x * 2 + 1, c.y)
      val v1 = v match {
        case Square2.Empty    => Square2.Empty
        case Square2.Wall     => Square2.Wall
        case Square2.SmallBox => Square2.LeftBox
        case _                => sys.error("unexpected")
      }
      val v2 = v match {
        case Square2.Empty    => Square2.Empty
        case Square2.Wall     => Square2.Wall
        case Square2.SmallBox => Square2.RightBox
        case _                => sys.error("unexpected")
      }
      field = field.updatedAtUnsafe(c1, v1)
      field = field.updatedAtUnsafe(c2, v2)
    }
    val state                   = State2(state1.robot.copy(x = state1.robot.x * 2), field)
    state.print("Mega state:")

    val result = directions.foldLeft(state) { (state, direction) =>
      val r = state.move(direction)
//      r.print(s"Move $direction:")
      r
    }
    result.print("Final state:")
    result.field.allCoords.map { c =>
      if (
        result.field.at(c).contains(Square2.LeftBox) | result.field
          .at(c)
          .contains(Square2.SmallBox)
      ) {
        100 * c.y + c.x
      } else {
        0
      }
    }.sum
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
