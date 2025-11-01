package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent17._

class Advent17Spec extends AnyFreeSpec {
  private def testData  = parseFile(fileName("-test"))
  private def testData2 = parseFile(fileName("-test-2"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 102
    }

    "real" in {
      part1(realData) shouldEqual 686
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 94
    }

    "test 2" in {
      part2(testData2) shouldEqual 71
    }

    "real" in {
      part2(realData) shouldEqual 801
    }
  }
}
