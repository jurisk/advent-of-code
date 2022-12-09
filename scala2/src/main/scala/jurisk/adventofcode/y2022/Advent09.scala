package jurisk.adventofcode.y2022

import jurisk.utils.FileInput._
import jurisk.utils.Geometry.{Coords2D, Direction2D}
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

  private def catchUpSegment(t: Coords2D, h: Coords2D): Coords2D = {
    val md = h.manhattanDistance(t)
    if (md < 2) { // close enough, no move
      t
    } else if (md == 2) {
      if ((t.x == h.x) || (t.y == h.y)) {
        Coords2D((t.x + h.x) / 2, (t.y + h.y) / 2)
      } else {
        t // diagonal is fine, no move
      }
    } else if (md == 3) {
      val diffX = t.x - h.x
      val diffY = t.y - h.y
      if (diffX.absInt > diffY.absInt) {
        Coords2D((t.x + h.x) / 2, h.y)
      } else if (diffX.absInt < diffY.absInt) {
        Coords2D(h.x, (t.y + h.y) / 2)
      } else {
        sys.error(s"Unexpected: $t $h")
      }
    } else if (md == 4) {
      Coords2D((h.x + t.x) / 2, (h.y + t.y) / 2)
    } else {
      sys.error(s"Unexpected MD: $t $h")
    }
  }

  private def catchUpRope(list: List[Coords2D]): List[Coords2D] =
    list match {
      case a :: b :: rest =>
        val newB = catchUpSegment(b, a)
        a :: catchUpRope(newB :: rest)
      case _              => list
    }

  case class Rope(elems: List[Coords2D]) {
    def applyMove(direction: Direction2D): Rope = {
      val newH     = elems.head + direction.diff
      val newElems = catchUpRope(newH :: elems.tail)
      Rope(newElems)
    }

    def last: Coords2D = elems.last
  }

  def process(parsed: Parsed): Processed = parsed

  def parse(fileName: String): Parsed =
    parseFileLines(fileName, Move.parse)

  private def ropeToString(rope: Rope, n: Int): String = {
    def charForIndex(idx: Int): Char = if (idx == 0) 'H' else ('0' + idx).toChar

    ((-n to n) map { y =>
      ((-n to n) map { x =>
        val c = Coords2D.of(x, y)

        val ropeChars = rope.elems.indices.toList map { idx =>
          if (rope.elems(idx) == c) Some(charForIndex(idx)) else None
        }

        val start = if (c == Coords2D.Zero) Some('s') else None

        (ropeChars ::: List(start)).flatten.headOption.getOrElse('·')
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
    val test1 = parse("2022/09-test-1.txt")
    val test2 = parse("2022/09-test-2.txt")
    val real  = parse("2022/09.txt")

    part1(test1) shouldEqual 13
    part1(real) shouldEqual 6486

    part2(test1) shouldEqual 1
    part2(test2) shouldEqual 36
    part2(real) shouldEqual 2678
  }
}
