package jurisk.adventofcode.y2022

import jurisk.utils.FileInput.readFileLines
import jurisk.utils.Utils.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent04 {
  case class Elf(from: Int, to: Int) {
    def fullyContains(other: Elf): Boolean =
      (from <= other.from) && (to >= other.to)

    def partiallyOverlaps(other: Elf): Boolean =
      ((from >= other.from) && (from <= other.to)) ||
        ((to >= other.from) && (to <= other.to))
  }

  object Elf {
    def parse(s: String): Elf = {
      val (a, b) = s.parseSeparatedPairUnsafe("-", _.toInt)
      Elf(a, b)
    }
  }

  case class Pair(first: Elf, second: Elf) {
    def fullyContained: Boolean =
      first.fullyContains(second) || second.fullyContains(first)

    def overlap: Boolean =
      first.partiallyOverlaps(second) || fullyContained
  }

  object Pair {
    def parse(s: String): Pair = {
      val (a, b) = s.parseSeparatedPairUnsafe(",", Elf.parse)
      Pair(a, b)
    }
  }

  type Parsed = List[Pair]
  type Result = Int

  def parse(fileName: String): Parsed =
    readFileLines(fileName) map Pair.parse

  def part1(data: Parsed): Result =
    data.count(_.fullyContained)

  def part2(data: Parsed): Result =
    data.count(_.overlap)

  def main(args: Array[String]): Unit = {
    val test = parse("2022/04-test.txt")
    val real = parse("2022/04.txt")

    part1(test) shouldEqual 2
    part1(real) shouldEqual 547

    part2(test) shouldEqual 4
    part2(real) shouldEqual 843
  }
}
