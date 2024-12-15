package jurisk.adventofcode.y2024

import Advent15._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent15Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "test 0" in {
      part1(testData0) shouldEqual 2028
    }

    "test 1" in {
      part1(testData1) shouldEqual 10092
    }

    "real" in {
      part1(realData) shouldEqual 1577255
    }
  }

  "part 2" - {
    "test" in {
      part2(testData1) shouldEqual 9021
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
