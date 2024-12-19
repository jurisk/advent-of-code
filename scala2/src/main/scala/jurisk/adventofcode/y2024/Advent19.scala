package jurisk.adventofcode.y2024

import jurisk.utils.CollectionOps.IterableOnceOps
import jurisk.utils.FileInput._
import jurisk.utils.Memoize
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

object Advent19 {
  final case class Input(
    stripes: List[String],
    towels: List[String],
  )
  type N = Long

  def parse(input: String): Input = {
    val (stripes, towels) = input.splitPairByDoubleNewline
    Input(stripes.commaSeparatedList, towels.splitLines)
  }

  def solve(data: Input, countF: N => N): N = {
    lazy val waysMemoized = Memoize.memoize1(ways)

    def ways(towel: String): N =
      if (towel.isEmpty) 1L
      else {
        data.stripes.flatMap { stripe =>
          towel.startsWith(stripe).option {
            waysMemoized(towel.drop(stripe.length))
          }
        }.sum
      }

    data.towels.sumBy(waysMemoized andThen countF)
  }

  def part1(data: Input): N =
    solve(data, _ min 1)

  def part2(data: Input): N =
    solve(data, identity)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/19$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
