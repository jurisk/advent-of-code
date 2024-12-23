package jurisk.adventofcode.y2024

import Advent23._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent23Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 7
    }

    "real" in {
      part1(realData) shouldEqual 1230
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual "co,de,ka,ta"
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
