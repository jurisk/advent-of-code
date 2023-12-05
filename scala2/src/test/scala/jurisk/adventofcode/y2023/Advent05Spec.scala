package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Advent05._

class Advent05Spec extends AnyFlatSpec {
  "Advent00" should "test part 1" in {
    part1(parseFile("2023/05-test.txt")) shouldEqual 35L
  }

  it should "real part 1" in {
    part1(parseFile("2023/05.txt")) shouldEqual 424490994L
  }

  it should "test part 2" in {
    part2(parseFile("2023/05-test.txt")) shouldEqual 46L
  }

  it should "real part 2" in {
    part2(parseFile("2023/05.txt")) shouldEqual 15290096L
  }
}
