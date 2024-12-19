package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent19 {
  final case class Input(
    stripes: List[String],
    towels: List[String],
  )
  type N     = Int

  def parse(input: String): Input = {
    val (stripes, towels) = input.split("\n\n").toList match {
      case List(stripes, towels) => (stripes.split(", ").toList, towels.linesIterator.toList)
      case _ => throw new Exception("Invalid input")
    }
    Input(stripes, towels)
  }

  private def isValid(towel: String, stripes: List[String]): Boolean = {
    if (towel.isEmpty) true
    else {
      stripes.exists { stripe =>
//        println(s"Towel $towel, Stripe $stripe, Starts: ${towel.startsWith(stripe)}")
        towel.startsWith(stripe) && isValid(towel.drop(stripe.length), stripes)
      }
    }
  }

  def part1(data: Input): N = {
    data.towels.count(towel => isValid(towel, data.stripes))
  }

  def part2(data: Input): N =
    0

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
