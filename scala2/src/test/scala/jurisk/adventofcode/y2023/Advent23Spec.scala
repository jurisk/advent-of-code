package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent23._
import org.scalatest.matchers.should.Matchers._

class Advent23Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def simpleData = parseFile(fileName("-test-simple"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 94
    }

    "real" in {
      part1(realData) shouldEqual 1966
    }
  }

  "part 2" - {
    "test simple" in {
      part2Crude(simpleData) shouldEqual part2(simpleData)
    }

    "test crude" in {
      part2Crude(testData) shouldEqual 154
    }

    "test" in {
      part2(testData) shouldEqual 154
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
