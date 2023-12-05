package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Advent01._

class Advent01Spec extends AnyFlatSpec {
  "Advent01" should "test part 1" in {
    part1(parseFile("2023/01-test-1.txt")) shouldEqual 142
  }

  it should "real part 1" in {
    part1(parseFile("2023/01.txt")) shouldEqual 54081
  }

  it should "test part 2" in {
    part2(parseFile("2023/01-test-2.txt")) shouldEqual 281
  }

  it should "real part 2" in {
    part2(parseFile("2023/01.txt")) shouldEqual 54649
  }
}
