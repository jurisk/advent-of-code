package jurisk.adventofcode.y2023

import jurisk.utils.CollectionOps.{EqIterableOps, IntegralSeqOps}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent09 {
  type N     = Long
  type Input = List[List[N]]

  private def extrapolatedValue(list: Seq[N]): N =
    if (list.allEqual(0)) 0 else list.last + extrapolatedValue(list.differences)

  def part1(input: Input): N = input.map(extrapolatedValue).sum
  def part2(input: Input): N = part1(input.map(_.reverse))

  def parse(input: String): Input = input.parseLines(_.extractLongList)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/09.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
