package jurisk.adventofcode.y2025

import jurisk.adventofcode.y2025.Advent11._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent11Spec extends AnyFreeSpec {
  private def testData1 = parseFile(fileName("-test-00"))
  private def testData2 = parseFile(fileName("-test-01"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData1) shouldEqual 5
    }

    "real" in {
      part1(realData) shouldEqual 643
    }
  }

  "part 2" - {
    "test" in {
      part2(testData2) shouldEqual 2
    }

    "real" in {
      part2(realData) shouldEqual 417190406827152L
    }
  }
}
