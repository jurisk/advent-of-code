package jurisk.adventofcode.y2024

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2024.Advent15.Square.Empty
import jurisk.adventofcode.y2024.Advent15.Square.LargeBox
import jurisk.adventofcode.y2024.Advent15.Square.LargeBox.Side
import jurisk.adventofcode.y2024.Advent15.Square.LargeBox.Side.LeftSide
import jurisk.adventofcode.y2024.Advent15.Square.LargeBox.Side.RightSide
import jurisk.adventofcode.y2024.Advent15.Square.SmallBox
import jurisk.adventofcode.y2024.Advent15.Square.Wall
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Direction2D.E
import jurisk.geometry.Direction2D.N
import jurisk.geometry.Direction2D.S
import jurisk.geometry.Direction2D.W
import jurisk.geometry.Direction2D.parseCaret
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent15 {
  type Input = (State, List[CardinalDirection2D])
  type N     = Long

  sealed trait Square extends Product with Serializable {
    def toChar: Char =
      this match {
        case Empty               => '.'
        case Wall                => '#'
        case SmallBox            => 'O'
        case LargeBox(LeftSide)  => '['
        case LargeBox(RightSide) => ']'
      }
  }

  object Square {
    object LargeBox {
      sealed trait Side extends Product with Serializable {
        def otherDirection: CardinalDirection2D
      }
      object Side {
        case object LeftSide  extends Side {
          val otherDirection: CardinalDirection2D = E
        }
        case object RightSide extends Side {
          val otherDirection: CardinalDirection2D = W
        }
      }
    }

    case object Empty                     extends Square
    case object Wall                      extends Square
    case object SmallBox                  extends Square
    final case class LargeBox(side: Side) extends Square

    def parse(c: Char): Square =
      c match {
        case '.' | '@' => Empty
        case '#'       => Wall
        case 'O'       => SmallBox
        case '['       => LargeBox(LeftSide)
        case ']'       => LargeBox(RightSide)
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
        field.at(n).contains(Empty) || coords.contains(n)
      }
      assert(valid, s"Cannot move coords $coords in direction $dir")
      val movePackageIsTemporarilyRemoved   = coords.foldLeft(field) { (f, c) =>
        f.updatedAtUnsafe(c, Empty)
      }
      val movePackageIsPlacedInNewPositions =
        coords.foldLeft(movePackageIsTemporarilyRemoved) { (f, c) =>
          val n = c + dir
          f.updatedAtUnsafe(n, field.at(c).getOrElse(s"No value at $c".fail))
        }
      State(robot, movePackageIsPlacedInNewPositions)
    }

    private def calculateMovePackage(
      c: Coords2D,
      direction: CardinalDirection2D,
    ): Option[List[Coords2D]] = {
      val next         = c + direction
      val verticalMove = Set(N, S) contains direction
      field.atOrElse(next, Wall) match {
        case Empty => Some(Nil)

        case Wall => None

        case LargeBox(side) if verticalMove =>
          val otherC = next + side.otherDirection
          (
            calculateMovePackage(next, direction),
            calculateMovePackage(otherC, direction),
          ) match {
            case (Some(a), Some(b)) => Some(next :: otherC :: a ::: b)
            case _                  => None
          }

        case LargeBox(_) | SmallBox =>
          calculateMovePackage(next, direction).map(next :: _)
      }
    }

    private def moveRobot(next: Coords2D): State =
      if (field.at(next) contains Empty) {
        State(next, field)
      } else {
        sys.error(s"Tried to move robot to $next but failed")
      }

    def move(direction: CardinalDirection2D): State = {
      val next        = robot + direction
      val movePackage = calculateMovePackage(robot, direction)
      movePackage match {
        case None              =>
          if (field.at(next) contains Empty) {
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
    val robot      =
      charField.findCoordsByValue('@').getOrElse("Robot not found".fail)
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
      if (Set(LargeBox(LeftSide), SmallBox) contains v) {
        100 * c.y + c.x
      } else {
        0
      }
    }.sum
  }

  def part2(data: Input): N = {
    val (incomingState, directions) = data

    val field = incomingState.field.flatMap {
      case Empty    =>
        Field2D.fromLists[Square](List(List(Empty, Empty)))
      case Wall     =>
        Field2D.fromLists[Square](List(List(Wall, Wall)))
      case SmallBox =>
        Field2D.fromLists[Square](
          List(List(LargeBox(LeftSide), LargeBox(RightSide)))
        )
      case other    => s"Unexpected value: $other".fail
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
