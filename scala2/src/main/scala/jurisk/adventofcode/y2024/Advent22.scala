package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent22 {
  type Input = List[N]
  type N     = Long

  def parse(input: String): Input =
    input.parseLines(_.toLong)

  def mixPrune(a: N, b: N): N =
    (a ^ b) % 16777216

  def next(n: N): N = {
    val a = mixPrune(n * 64, n)
    val b = mixPrune(a / 32, a)
    val c = mixPrune(b * 2048, b)
    c
  }

  def nthSecretNumber(n: N, nth: Int): N = {
    var current = n
    for (_ <- 0 until nth)
      current = next(current)
    current
  }

  def part1(data: Input): N =
    data.map(n => nthSecretNumber(n, 2000)).sum

  def part2(data: Input): N =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/22$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
