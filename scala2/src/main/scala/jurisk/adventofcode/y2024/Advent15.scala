package jurisk.adventofcode.y2024

import cats.implicits.toFunctorOps
import jurisk.geometry.Direction2D.{CardinalDirection2D, E, N, S, W, parseCaret}
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent15 {
  type Input = (State, List[CardinalDirection2D])
  type N     = Long

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
      val valid                             = coords.forall { c =>
        val n = c + dir
        field.at(n).contains(Square.Empty) || coords.contains(n)
      }
      assert(valid, s"Cannot move coords $coords in direction $dir")
      val movePackageIsTemporarilyRemoved   = coords.foldLeft(field) { (f, c) =>
        f.updatedAtUnsafe(c, Square.Empty)
      }
      val movePackageIsPlacedInNewPositions =
        coords.foldLeft(movePackageIsTemporarilyRemoved) { (f, c) =>
          val n = c + dir
          f.updatedAtUnsafe(n, field.at(c).get)
        }
      State(robot, movePackageIsPlacedInNewPositions)
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
        sys.error(s"Tried to move robot to $next but failed")
      }

    def move(direction: CardinalDirection2D): State = {
      val next        = robot + direction
      val movePackage = calculateMovePackage(robot, direction)
      movePackage match {
        case None              =>
          if (field.at(next) contains Square.Empty) {
            moveRobot(next)
          } else {
            this
          }
        case Some(movePackage) =>
          moveMany(movePackage, direction)
            .moveRobot(next)
      }
    }
  }

  def parse(input: String): Input = {
    val (a, b)     = input.splitPairByDoubleNewline
    val charField  = Field2D.parseCharField(a)
    val robot      = charField.findCoordsByValue('@').get
    val field      = charField.map(Square.parse)
    val state      = State(robot, field)
    val dirs       = b.splitLines.mkString
    val directions = dirs.map(parseCaret).toList
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

    val field = incomingState.field.flatMap {
      case Square.Empty    =>
        Field2D.fromLists[Square](List(List(Square.Empty, Square.Empty)))
      case Square.Wall     =>
        Field2D.fromLists[Square](List(List(Square.Wall, Square.Wall)))
      case Square.SmallBox =>
        Field2D.fromLists[Square](List(List(Square.LeftBox, Square.RightBox)))
      case other           => s"Unexpected value: $other".fail
    }

    val newRobot = Coords2D(incomingState.robot.x * 2, incomingState.robot.y)
    val state    = State(newRobot, field)

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
