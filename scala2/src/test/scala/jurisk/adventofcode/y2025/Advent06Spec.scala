package jurisk.adventofcode.y2025

import jurisk.adventofcode.y2025.Advent06._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent06Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 4277556
    }

    "real" in {
      part1(realData) shouldEqual 4878670269096L
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 3263827
    }

    "real" in {
      part2(realData) shouldEqual 8674740488592L
    }
  }
}
