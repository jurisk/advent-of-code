package jurisk.adventofcode.y2025

import jurisk.adventofcode.y2025.Advent11._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent11Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 5
    }

    "real" in {
      part1(realData) shouldEqual 643
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 2
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
