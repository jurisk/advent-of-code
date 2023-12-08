package jurisk.adventofcode.y2022

import jurisk.utils.FileInput.readSingleFileLine
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent06 {
  type Parsed = String

  def readFileAndParse(fileName: String): Parsed =
    readSingleFileLine(fileName)

  def solve(data: Parsed, n: Int): Int = {
    val found = data.sliding(n).indexWhere(_.toList.allDistinct)
    require(found != -1)
    found + n
  }

  def part1(data: Parsed): Int = solve(data, 4)

  def part2(data: Parsed): Int = solve(data, 14)

  def main(args: Array[String]): Unit = {
    val test = readFileAndParse("2022/06-test.txt")
    val real = readFileAndParse("2022/06.txt")

    part1(test) shouldEqual 7
    part1(real) shouldEqual 1707

    part2(test) shouldEqual 19
    part2(real) shouldEqual 3697
  }
}
