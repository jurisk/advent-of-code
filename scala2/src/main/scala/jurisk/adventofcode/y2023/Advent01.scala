package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import scala.annotation.tailrec

object Advent01 {
  type Parsed = List[String]

  def parse(input: String): Parsed =
    input.split('\n').toList

  private def stringToDigitList(
    mappings: List[(String, Int)]
  )(s: String): List[Int] = {
    @tailrec
    def f(remaining: String, acc: List[Int]): List[Int] =
      remaining.headOption match {
        case Some(digitChar) if digitChar.isDigit =>
          f(remaining.tail, (digitChar - '0') :: acc)

        case Some(nonDigitChar @ _) =>
          // https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm would be more efficient
          val digitFromLetters = mappings
            .find { case (mapping, _) =>
              remaining startsWith mapping
            }
            .map { case (_, digit) => digit }

          f(remaining.tail, digitFromLetters.fold(acc)(_ :: acc))

        case None =>
          acc.reverse
      }

    f(s, Nil)
  }

  private def solve(data: Parsed, stringToInts: String => List[Int]): Int = {
    def calibrationValue(ints: List[Int]): Int = ints.head * 10 + ints.last

    data.map(stringToInts andThen calibrationValue).sum
  }

  def part1(data: Parsed): Int =
    solve(data, stringToDigitList(Nil))

  def part2(data: Parsed): Int = {
    val mappings = List(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
    ).zip(1 to 9)

    solve(data, stringToDigitList(mappings))
  }

  private[y2023] def parseFile(fileName: String): Parsed =
    readFileLines(fileName)

  def main(args: Array[String]): Unit = {
    val realData: Parsed = parseFile("2023/01.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
