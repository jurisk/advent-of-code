package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import org.scalatest.matchers.should.Matchers._

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

  def main(args: Array[String]): Unit = {
    val testData1 = readFileText("2023/01-test-1.txt")
    val testData2 = readFileText("2023/01-test-2.txt")
    val realData  = readFileText("2023/01.txt")

    val test1 = parse(testData1)
    val test2 = parse(testData2)
    val real  = parse(realData)

    part1(test1) shouldEqual 142
    part1(real) shouldEqual 54081

    part2(test2) shouldEqual 281
    part2(real) shouldEqual 54649
  }
}
