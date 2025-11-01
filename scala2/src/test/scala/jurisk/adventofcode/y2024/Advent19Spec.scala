package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent19._

class Advent19Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 6
    }

    "real" in {
      part1(realData) shouldEqual 374
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 16
    }

    "real" in {
      part2(realData) shouldEqual 1100663950563322L
    }
  }
}
