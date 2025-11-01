package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent04._

class Advent04Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 18
    }

    "real" in {
      part1(realData) shouldEqual 2468
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 9
    }

    "real" in {
      part2(realData) shouldEqual 1864
    }
  }
}
