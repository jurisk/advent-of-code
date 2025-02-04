package jurisk.adventofcode.y2022

import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.CollectionOps.IteratorOps
import jurisk.utils.FileInput.readSingleFileLine
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent06 {
  type Parsed = String

  def readFileAndParse(fileName: String): Parsed =
    readSingleFileLine(fileName)

  def solve(data: Parsed, n: Int): Int =
    data
      .sliding(n)
      .firstIndexWhere(_.toList.allDistinct)
      .getOrElse("Not found".fail) + n

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
