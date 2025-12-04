package jurisk.adventofcode.y2025

import cats.implicits._
import jurisk.math.pow
import jurisk.utils.FileInput._
import jurisk.utils.Memoize
import jurisk.utils.Parsing.StringOps

object Advent03 {
  private type Battery = Short
  private type Bank    = List[Battery]
  type Input           = List[Bank]
  type N               = Long

  def parse(input: String): Input =
    input.parseLines { s =>
      s.toList.map(n => (n - '0').toShort)
    }

  private val joltsMemoized = Memoize.memoize2(jolts)

  private[y2025] def jolts(bank: Bank, digitsLeft: Int): Option[N] =
    if (digitsLeft == 0) {
      0L.some
    } else {
      bank match {
        case head :: tail =>
          List(
            joltsMemoized(tail, digitsLeft - 1).map { usingHeadResult =>
              head * pow(10, digitsLeft - 1) + usingHeadResult
            },
            joltsMemoized(tail, digitsLeft),
          ).flatten.maxOption
        case Nil          => none
      }
    }

  private def solve(data: Input, digits: Int): N =
    data.flatMap { bank =>
      jolts(bank, digits)
    }.sum

  def part1(data: Input): N =
    solve(data, 2)

  def part2(data: Input): N =
    solve(data, 12)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/03$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
