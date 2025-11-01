package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent10._

class Advent10Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 36
    }

    "real" in {
      part1(realData) shouldEqual 811
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 81
    }

    "real" in {
      part2(realData) shouldEqual 1794
    }
  }
}
