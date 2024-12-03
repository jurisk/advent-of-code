package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

import scala.util.matching.Regex

object Advent03 {
  type Input = String

  private val Number = """(\d+)""".r
  private val Mul    = s"""mul\\($Number,$Number\\)""".r
  private val Do     = """do\(\)""".r
  private val Dont   = """don't\(\)""".r

  def solve(options: Regex)(data: Input): Int = {
    val matches     = options.findAllIn(data)
    val (result, _) = matches.foldLeft((0, true)) {
      case ((result, active), m) =>
        m.trim match {
          case Mul(a, b) =>
            (result + active.fold(a.toInt * b.toInt, 0), active)
          case Do()      =>
            (result, true)
          case Dont()    =>
            (result, false)
          case _         =>
            m.fail
        }
    }

    result
  }

  def part1: Input => Int = solve(Mul)
  def part2: Input => Int = solve(List(Mul, Do, Dont).mkString("|").r)

  def fileName(suffix: String): String =
    s"2024/03$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = readFileText(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
