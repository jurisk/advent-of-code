package jurisk.adventofcode.y2018

import org.scalatest.freespec.AnyFreeSpec
import Advent23._
import org.scalatest.matchers.should.Matchers._

class Advent23Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2018/23-test-1.txt")) shouldEqual 7
    }

    "real" in {
      part1(parseFile("2018/23.txt")) shouldEqual 393
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2018/23-test-1.txt")) shouldEqual 36
    }

    "real" in {
      part2(parseFile("2018/23.txt")) shouldEqual 113799398
    }
  }
}
