package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Advent09._

class Advent09Spec extends AnyFlatSpec {
  "Advent09" should "test part 1" in {
    part1(parseFile("2023/09-test.txt")) shouldEqual 114
  }

  it should "real part 1" in {
    part1(parseFile("2023/09.txt")) shouldEqual 1684566095
  }

  it should "test part 2" in {
    part2(parseFile("2023/09-test.txt")) shouldEqual 2
  }

  it should "real part 2" in {
    part2(parseFile("2023/09.txt")) shouldEqual 1136
  }
}
