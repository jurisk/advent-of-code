package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import Advent02._

class Advent02Spec extends AnyFlatSpec {
  "Advent02" should "test part 1" in {
    part1(parseFile("2023/02-test.txt")) shouldEqual 8
  }

  it should "test part 2" in {
    part2(parseFile("2023/02-test.txt")) shouldEqual 2286
  }

  it should "real part 1" in {
    part1(parseFile("2023/02.txt")) shouldEqual 2164
  }

  it should "real part 2" in {
    part2(parseFile("2023/02.txt")) shouldEqual 69929
  }
}
