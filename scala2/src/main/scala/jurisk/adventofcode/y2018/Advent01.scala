package jurisk.adventofcode.y2018

import jurisk.utils.FileInput.parseFileLines
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent01 {
  type Parsed = List[Int]
  type Result = Int

  private def readFileAndParse(fileName: String): Parsed =
    parseFileLines(fileName, _.toInt)

  def part1(data: Parsed): Result =
    data.sum

  def part2(data: Parsed): Result = {
    @tailrec
    def f(index: Int, counter: Int, seen: Set[Int]): Int = {
      val newCounter = counter + data(index % data.length)
      if (seen.contains(newCounter)) {
        newCounter
      } else {
        f(index + 1, newCounter, seen + newCounter)
      }
    }

    f(0, 0, Set.empty)
  }

  def main(args: Array[String]): Unit = {
    val real = readFileAndParse("2018/01.txt")

    part1(real) shouldEqual 474
    part2(real) shouldEqual 137041
  }
}
