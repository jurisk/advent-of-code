package jurisk

import jurisk.Utils.readLineGroups
import org.scalatest.matchers.should.Matchers._

object Advent01 {
  def parse(fileName: String): List[List[Int]] =
    readLineGroups(fileName) map { section =>
      section.map(_.toInt)
    }

  def process(data: List[List[Int]]): List[Int] =
    data.map(_.sum).sorted(Ordering[Int].reverse)

  def part1(data: List[List[Int]]): Int =
    process(data).head

  def part2(data: List[List[Int]]): Int =
    process(data).take(3).sum

  def main(args: Array[String]): Unit = {
    val test = parse("01-test.txt")
    part1(test) shouldEqual 24000
    part2(test) shouldEqual 45000

    val real = parse("01.txt")
    part1(real) shouldEqual 74394
    part2(real) shouldEqual 212836
  }
}
