package jurisk.adventofcode

import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import jurisk.utils.FileInput._
import org.scalatest.matchers.should.Matchers._

object AdventTemplate {
  type Parsed    = List[Entry]
  type Processed = Parsed
  type Result1   = Int
  type Result2   = String

  final case class Entry(
    value: String
  )

  object Entry {
    def parse(s: String): Entry =
      Entry(s.splitPairUnsafe(" ").toString)
  }

  def process(parsed: Parsed): Processed = parsed

  def parse(data: String): Parsed =
    data.parseList("\n", Entry.parse)

  def part1(data: Parsed): Result1 = {
    val processed = process(data)
    processed.length
  }

  def part2(data: Parsed): Result2 = {
    val processed = process(data)
    processed.counts.toString
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/00-test.txt")
    val realData = readFileText("2022/00.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 12345678
    part1(real) shouldEqual 12345678

    part2(test) shouldEqual "asdf"
    part2(real) shouldEqual "asdf"
  }
}
