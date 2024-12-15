package jurisk.adventofcode.y2024

import Advent15._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent15Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def testData2 = parseFile(fileName("-test-02"))
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
    "test 1" in {
      part2(testData1) shouldEqual 9021
    }

    "test 2" in {
      part2(testData2) shouldEqual 618
    }

    // Not 1582584
    "real" in {
      part2(realData) shouldEqual 1597035
    }
  }
}
