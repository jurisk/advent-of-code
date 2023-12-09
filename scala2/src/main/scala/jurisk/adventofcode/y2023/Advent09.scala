package jurisk.adventofcode.y2023

import jurisk.utils.CollectionOps.{EqIterableOps, IntegralIterableOps}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent09 {
  type N     = Long
  type Input = List[List[N]]

  private def extrapolatedValue(
    direction: (Iterable[N], N) => N
  )(list: Iterable[N]): N =
    if (list.allEqual(0)) 0
    else direction(list, extrapolatedValue(direction)(list.differencesUnsafe))

  def solve(direction: (Iterable[N], N) => N): Input => N =
    _.map(extrapolatedValue(direction)).sum

  val part1: Input => N = solve { case (thisLevel, extrapolatedValue) =>
    thisLevel.last + extrapolatedValue
  }

  val part2: Input => N = solve { case (thisLevel, extrapolatedValue) =>
    thisLevel.head - extrapolatedValue
  }

  val parse: String => Input =
    _.parseLines(_.extractLongs)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/09.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
