package jurisk.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import Advent00._
import org.scalatest.matchers.should.Matchers._

class Advent00Spec extends AnyFlatSpec {
  "Advent00" should "test part 1" in {
    part1(parseFile("2023/02-test.txt")) shouldEqual 5 + 1234567
  }

  it should "real part 1" in {
    part1(parseFile("2023/02.txt")) shouldEqual 100 + 1234567
  }

  it should "test part 2" in {
    part2(parseFile("2023/02-test.txt")) shouldEqual 5 + 1234567
  }

  it should "real part 2" in {
    part2(parseFile("2023/02.txt")) shouldEqual 100 + 1234567
  }
}
