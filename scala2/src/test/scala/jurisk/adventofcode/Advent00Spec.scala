package jurisk.adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import Advent00._
import org.scalatest.matchers.should.Matchers._

class Advent00Spec extends AnyFlatSpec {
  "Advent00" should "test part 1" ignore {
    part1(parseFile("0000/00-test.txt")) shouldEqual 12345678
  }

  it should "test part 2" ignore {
    part2(parseFile("0000/00-test.txt")) shouldEqual 12345678
  }

  it should "real part 1" ignore {
    part1(parseFile("0000/00.txt")) shouldEqual 12345678
  }

  it should "real part 2" ignore {
    part2(parseFile("0000/00.txt")) shouldEqual 12345678
  }
}
