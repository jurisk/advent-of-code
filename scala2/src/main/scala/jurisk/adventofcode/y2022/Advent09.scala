package jurisk.adventofcode.y2022

import cats.data.State
import jurisk.utils.FileInput._
import jurisk.utils.Geometry.Coords2D
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.HashSet

object Advent09 {
  type Parsed    = List[Move]
  type Processed = Parsed
  type Result1   = Int
  type Result2   = Int

  sealed trait Direction
  object Direction {
    case object Up extends Direction
    case object Down extends Direction
    case object Left extends Direction
    case object Right extends Direction

    def parse(s: String): Direction = s match {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }
  }

  final case class Move(
    direction: Direction,
    amount: Int
  ) {
    def diff: Coords2D = direction match {
      case Direction.Up    => Coords2D.of(0, -amount)
      case Direction.Down  => Coords2D.of(0, amount)
      case Direction.Left  => Coords2D.of(-amount, 0)
      case Direction.Right => Coords2D.of(+amount, 0)
    }
  }

  object Move {
    def parse(s: String): Move = {
      val (direction, amount) = s.splitPairUnsafe(" ")
      Move(Direction.parse(direction), amount.toInt)
    }
  }

  def catchUpTail(t: Coords2D, newH: Coords2D): Coords2D = {
    val md = newH.manhattanDistance(t)
    if (md < 2) {
      t
    } else if (md == 2) {
      if (t.x == newH.x) {
        if (t.y.value < newH.y.value) {
          Coords2D.of(t.x.value, t.y.value + 1)
        } else {
          Coords2D.of(t.x.value, t.y.value - 1)
        }
      } else if (t.y == newH.y) {
        if (t.x.value < newH.x.value) {
          Coords2D.of(t.x.value + 1, t.y.value)
        } else if (t.x.value > newH.x.value) {
          Coords2D.of(t.x.value - 1, t.y.value)
        } else { // diagonal
          sys.error(s"asdf $t $newH")
        }
      } else { // diagonal
        t
      }
    } else if (md == 3) {
      val diffX = t.x.value - newH.x.value
      val diffY = t.y.value - newH.y.value
      if (Math.abs(diffX) > Math.abs(diffY)) {
        Coords2D.of((t.x.value + newH.x.value) / 2, newH.y.value)
      } else if (Math.abs(diffX) < Math.abs(diffY)) {
        Coords2D.of(newH.x.value, (t.y.value + newH.y.value) / 2)
      } else {
        sys.error(s"asdf $t $newH")
      }
    } else {
      sys.error(s"Wrong MD: $t, $newH")
    }
  }

  case class State(h: Coords2D, t: Coords2D) {
    def applyMove(m: Move): (State, List[Coords2D]) = {
      val newH = h + m.diff
      val newT = catchUpTail(t, newH)
      val newState = State(newH, newT)
      (newState, newT :: Nil)
    }
  }

  def process(parsed: Parsed): Processed = parsed

  def parse(fileName: String): Parsed =
    parseFileLines(fileName, Move.parse)

  def printState(state: State): Unit = {
    val N = 5
    (-N to N) foreach { y =>
      (-N to N) foreach { x =>
        val c = Coords2D.of(x, y)
        val ch = if ((state.t == c) && (state.h == c)) {
          '!'
        } else if ((state.t == c)) {
          'T'
        } else if (state.h == c) {
          'H'
        } else {
          if (c == Coords2D.Zero) {
            's'
          } else {
            '·'
          }
        }
        print(ch)
      }
      println
    }
    println
  }

  def part1(data: Parsed): Result1 = {
    var visited: HashSet[Coords2D] = HashSet(Coords2D.Zero)
    var state: State = State(Coords2D.Zero, Coords2D.Zero)
    data foreach { move =>
      println(s"Applying $move")
      (0 until move.amount).foreach { _ =>
        printState(state)
        val (newState, tailVisits) = state.applyMove(move.copy(amount = 1))
        state = newState
        tailVisits.foreach { what =>
          visited = visited + what
        }
      }
    }

    println(state)

    visited.size
  }

  def part2(data: Parsed): Result2 = {
    val processed = process(data)
    processed.size
  }

  def main(args: Array[String]): Unit = {
    val test = parse("2022/09-test.txt")
    val real = parse("2022/09.txt")

    part1(test) shouldEqual 13
    part1(real) shouldEqual 6486

    part2(test) shouldEqual 36
    part2(real) shouldEqual "asdf"
  }
}
