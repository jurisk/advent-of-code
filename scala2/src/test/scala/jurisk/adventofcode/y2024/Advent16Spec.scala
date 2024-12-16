package jurisk.adventofcode.y2024

import Advent16._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent16Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "test 0" in {
      part1(testData0) shouldEqual 7036
    }

    "test 1" in {
      part1(testData1) shouldEqual 11048
    }

    "real" in {
      part1(realData) shouldEqual 74392
    }
  }

  "part 2" - {
    "test 0" in {
      part2(testData0) shouldEqual 45
    }

    "test 1" in {
      part2(testData1) shouldEqual 64
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
