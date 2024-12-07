package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent07 {
  type Input = List[Equation]

  private def concatLongs(a: Long, b: Long): Long = {
    val bLength = math.log10(b.toDouble).toInt + 1
    a * math.pow(10, bLength).toLong + b
  }

  private[y2024] def validate(
    allowPipe: Boolean,
    result: Long,
    numbers: List[Long],
  ): Boolean =
    numbers match {
      case Nil         => false
      case a :: Nil    => result == a
      case a :: b :: t =>
        val options = List(
          a * b,
          a + b,
        ) ::: (if (allowPipe) List(concatLongs(a, b)) else Nil)

        options.exists { r =>
          validate(allowPipe, result, r :: t)
        }
    }

  final case class Equation(
    result: Long,
    numbers: List[Long],
  ) {
    def options1: Boolean =
      validate(allowPipe = false, result, numbers)

    def options2: Boolean =
      validate(allowPipe = true, result, numbers)
  }

  object Equation {
    def parse(s: String): Equation = {
      val (a, b) = s.parsePairUnsafe(": ", _.toLong, _.parseList(" ", _.toLong))
      Equation(a, b)
    }
  }

  def parse(input: String): Input =
    input.parseLines(Equation.parse)

  def part1(data: Input): Long =
    data.filter(_.options1).map(_.result).sum

  def part2(data: Input): Long =
    data.filter(_.options2).map(_.result).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/07$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
