package jurisk.adventofcode.y2025

import jurisk.adventofcode.y2025.Advent08._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent08Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData, 10, 3) shouldEqual 5 * 4 * 2
    }

    "real" in {
      part1(realData, 1000, 3) shouldEqual 330786
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 25272
    }

    "real" in {
      part2(realData) shouldEqual 3276581616L
    }
  }
}
