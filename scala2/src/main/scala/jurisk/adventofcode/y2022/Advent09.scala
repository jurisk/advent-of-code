package jurisk.adventofcode.y2022

import jurisk.utils.FileInput._
import jurisk.utils.Geometry.{Coords2D, Direction2D, X, Y}
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.HashSet

object Advent09 {
  type Parsed    = List[Move]
  type Processed = Parsed
  type Result    = Int

  final case class Move(
    direction: Direction2D,
    amount: Int,
  )

  object Move {
    def parse(s: String): Move = {
      val (direction, amount) = s.splitPairUnsafe(" ")
      Move(Direction2D.parseUDLR(direction), amount.toInt)
    }
  }

  object Rope {
    private def catchUpSegment(t: Coords2D, h: Coords2D): Coords2D = {
      def f(a: Int, b: Int): Int =
        Math.abs(a - b) match {
          case 0 => a
          case 1 => b
          case 2 => (a + b) / 2
          case _ => sys.error(s"Unexpectedly large distance: $a $b")
        }

      val md                   = h.manhattanDistance(t)
      val overlapping          = md == 0
      val neighbouring         = md == 1
      val diagonalNeighbouring = (md == 2) && (t.x != h.x) && (t.y != h.y)

      if (overlapping || neighbouring || diagonalNeighbouring) {
        t // close enough, no move
      } else {
        Coords2D.of(f(t.x.value, h.x.value), f(t.y.value, h.y.value))
      }
    }

    private def catchUpRope(list: List[Coords2D]): List[Coords2D] =
      list match {
        case a :: b :: rest =>
          val newB = catchUpSegment(b, a)
          a :: catchUpRope(newB :: rest)
        case _              => list
      }
  }

  case class Rope(elems: List[Coords2D]) {
    def applyMove(direction: Direction2D): Rope = {
      val newH     = elems.head + direction.diff
      val newElems = Rope.catchUpRope(newH :: elems.tail)
      Rope(newElems)
    }

    def last: Coords2D = elems.last
  }

  def process(parsed: Parsed): Processed = parsed

  def readFileAndParse(fileName: String): Parsed =
    parseFileLines(fileName, Move.parse)

  private def ropeToString(rope: Rope, n: Int): String = {
    def charForIndex(idx: Int): Char = if (idx == 0) 'H' else ('0' + idx).toChar

    ((-n to n) map { y =>
      ((-n to n) map { x =>
        val c = Coords2D.of(x, y)

        val ropeChars = rope.elems.indices.toVector map { idx =>
          if (rope.elems(idx) == c) Some(charForIndex(idx)) else None
        }

        val start = if (c == Coords2D.Zero) Some('s') else None

        (ropeChars :+ start).flatten.headOption.getOrElse('·')
      }).mkString
    }).map(x => s"$x\n").mkString
  }

  def solve(data: Parsed, n: Int): Result = {
    val initialVisited: HashSet[Coords2D] = HashSet(Coords2D.Zero)
    val initialRope: Rope                 = Rope(List.fill(n)(Coords2D.Zero))

    val (_, resultVisited) = data.foldLeft((initialRope, initialVisited)) {
      (acc, move) =>
        val (rope, visited) = acc

        (0 until move.amount).foldLeft((rope, visited)) { (acc, _) =>
          val (rope, visited) = acc
          val newRope         = rope.applyMove(move.direction)
          val newVisited      = visited + newRope.last
          (newRope, newVisited)
        }
    }

    resultVisited.size
  }

  def part1(data: Parsed): Result = solve(data, 2)

  def part2(data: Parsed): Result = solve(data, 10)

  def main(args: Array[String]): Unit = {
    val test1 = readFileAndParse("2022/09-test-1.txt")
    val test2 = readFileAndParse("2022/09-test-2.txt")
    val real  = readFileAndParse("2022/09.txt")

    part1(test1) shouldEqual 13
    part1(real) shouldEqual 6486

    part2(test1) shouldEqual 1
    part2(test2) shouldEqual 36
    part2(real) shouldEqual 2678
  }
}
