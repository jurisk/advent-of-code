package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent13._
import org.scalatest.matchers.should.Matchers._

class Advent13Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2023/13-test.txt")) shouldEqual 405
    }

    "real" in {
      part1(parseFile("2023/13.txt")) shouldEqual 36448
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/13-test.txt")) shouldEqual 0
    }

    "real" in {
      part2(parseFile("2023/13.txt")) shouldEqual 0
    }
  }
}
