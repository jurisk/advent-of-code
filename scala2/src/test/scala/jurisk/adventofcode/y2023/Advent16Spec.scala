package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent16._
import org.scalatest.matchers.should.Matchers._

class Advent16Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2023/16-test.txt")) shouldEqual 46
    }

    "real" in {
      part1(parseFile("2023/16.txt")) shouldEqual 7415
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/16-test.txt")) shouldEqual 51
    }

    "real" in {
      part2(parseFile("2023/16.txt")) shouldEqual 0
    }
  }
}
