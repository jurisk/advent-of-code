package jurisk.adventofcode.y2023

import Advent08._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Advent08Spec extends AnyFlatSpec {
  "Advent08" should "test part 1" in {
    part1(parseFile("2023/08-test-1.txt")) shouldEqual 6
  }

  it should "real part 1" in {
    part1(parseFile("2023/08.txt")) shouldEqual 20659
  }

  it should "test part 2" in {
    part2(parseFile("2023/08-test-2.txt")) shouldEqual 6
  }

  it should "real part 2" in {
    part2(parseFile("2023/08.txt")) shouldEqual 15690466351717L
  }
}
