package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import Advent04._

class Advent04Spec extends AnyFlatSpec {
  "Advent04" should "test part 1" in {
    part1(parseFile("2023/04-test.txt")) shouldEqual 13
  }

  it should "real part 1" in {
    part1(parseFile("2023/04.txt")) shouldEqual 27454
  }

  it should "test part 2" in {
    part2(parseFile("2023/04-test.txt")) shouldEqual 30
  }

  it should "real part 2" in {
    part2(parseFile("2023/04.txt")) shouldEqual 6857330
  }
}
