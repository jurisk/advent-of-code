package jurisk.adventofcode.y2024

import Advent08._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent08Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 14
    }

    "real" in {
      part1(realData) shouldEqual 256
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 34
    }

    "real" in {
      part2(realData) shouldEqual 1005
    }
  }
}
