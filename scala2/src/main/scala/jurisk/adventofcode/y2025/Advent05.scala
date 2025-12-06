package jurisk.adventofcode.y2025

import cats.implicits._
import jurisk.math.DiscreteInterval
import jurisk.math.DiscreteIntervalSet
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent05 {
  private type Ingredient = Long
  type Input              = (List[DiscreteInterval[Ingredient]], List[Ingredient])
  type N                  = Long

  def parse(input: String): Input = {
    val (a, b)      = input.splitPairByDoubleNewline
    val ranges      = a.splitLines.map { line =>
      val (from, to) = line.splitPairUnsafe('-').bimap(_.toLong, _.toLong)
      DiscreteInterval.inclusive(from, to)
    }
    val ingredients = b.splitLines.map(_.toLong)
    (ranges, ingredients)
  }

  def part1(data: Input): N = {
    val (ranges, ingredients) = data
    val set                   = DiscreteIntervalSet.from(ranges)
    ingredients.count(ingredient => set.contains(ingredient))
  }

  def part2(data: Input): N = {
    val (ranges, _) = data
    val set         = DiscreteIntervalSet.from(ranges)
    set.size
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/05$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
