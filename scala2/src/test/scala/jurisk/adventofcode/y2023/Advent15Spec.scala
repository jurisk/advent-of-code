package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent15._
import org.scalatest.matchers.should.Matchers._

class Advent15Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2023/15-test.txt")) shouldEqual 0
    }

    "real" in {
      part1(parseFile("2023/15.txt")) shouldEqual 0
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/15-test.txt")) shouldEqual 0
    }

    "real" in {
      part2(parseFile("2023/15.txt")) shouldEqual 0
    }
  }
}
