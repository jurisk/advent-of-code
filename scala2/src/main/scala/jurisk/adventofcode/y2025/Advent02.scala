package jurisk.adventofcode.y2025

import jurisk.math.DiscreteInterval
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent02 {
  type Input = List[DiscreteInterval[Long]]
  type N     = Long

  def parse(input: String): Input =
    input.parseList(',', _.parsePairUnsafe('-', _.toLong, _.toLong)).map {
      case (a, b) => DiscreteInterval.inclusive(a, b)
    }

  private def isInvalid(n: Long, m: Int): Boolean = {
    val s = n.toString
    if (s.length % m == 0) {
      val q = s.length / m
      s.grouped(q).toSet.size == 1
    } else {
      false
    }
  }

  private def isInvalid1(n: Long): Boolean =
    isInvalid(n, 2)

  private val MAX                          = Long.MaxValue.toString.length
  private def isInvalid2(n: Long): Boolean =
    (2 to MAX) exists { m =>
      isInvalid(n, m)
    }

  private def solve(data: Input, isInvalid: N => Boolean): N =
    data
      .flatMap {
        _.toSeq
      }
      .filter(isInvalid)
      .sum

  def part1(data: Input): N =
    solve(data, isInvalid1)

  def part2(data: Input): N =
    solve(data, isInvalid2)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/02$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
