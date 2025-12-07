package jurisk.adventofcode.y2025

import jurisk.adventofcode.y2025.Advent07._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent07Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 21
    }

    "real" in {
      part1(realData) shouldEqual 1533
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 40
    }

    "real" in {
      part2(realData) shouldEqual 10733529153890L
    }
  }
}
