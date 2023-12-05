package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Advent03._

class Advent03Spec extends AnyFlatSpec {
  "Advent03" should "test part 1" in {
    part1(parseFile("2023/03-test.txt")) shouldEqual 4361
  }

  it should "real part 1" in {
    part1(parseFile("2023/03.txt")) shouldEqual 533784
  }

  it should "test part 2" in {
    part2(parseFile("2023/03-test.txt")) shouldEqual 467835
  }

  it should "real part 2" in {
    part2(parseFile("2023/03.txt")) shouldEqual 78826761
  }
}
