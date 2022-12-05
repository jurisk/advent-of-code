package jurisk.adventofcode.y2022

import jurisk.utils.FileInput.parseLineGroups
import org.scalatest.matchers.should.Matchers._

object Advent01 {
  type Parsed = List[List[Int]]
  type Result = Int

  def parse(fileName: String): Parsed =
    parseLineGroups(fileName, _.map(_.toInt))

  private def process(data: Parsed): List[Int] =
    data.map(_.sum).sorted(Ordering[Int].reverse)

  def part1(data: Parsed): Result =
    process(data).head

  def part2(data: Parsed): Result =
    process(data).take(3).sum

  def main(args: Array[String]): Unit = {
    val test = parse("2022/01-test.txt")
    val real = parse("2022/01.txt")

    part1(test) shouldEqual 24000
    part1(real) shouldEqual 74394

    part2(test) shouldEqual 45000
    part2(real) shouldEqual 212836
  }
}
