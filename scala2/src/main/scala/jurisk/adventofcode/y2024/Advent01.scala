package jurisk.adventofcode.y2024

import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent01 {
  type Input = (List[Int], List[Int])

  private val Line                = """(\d+)\s+(\d+)""".r
  def parse(input: String): Input =
    input.parseLines {
      case Line(first, second) => (first.toInt, second.toInt)
      case other               => other.failedToParse
    }.unzip

  def part1(data: Input): Int = {
    val (a, b)             = data
    val (aSorted, bSorted) = (a.sorted, b.sorted)
    val zipped             = aSorted zip bSorted
    val diffs              = zipped map { case (x, y) => (x - y).abs }
    diffs.sum
  }

  def part2(data: Input): Int = {
    val (a, b) = data
    val counts = b.counts

    a.map { x =>
      x * counts.getOrElse(x, 0)
    }.sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/01$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
