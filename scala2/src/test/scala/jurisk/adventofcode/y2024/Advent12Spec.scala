package jurisk.adventofcode.y2024

import Advent12._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent12Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def testData2 = parseFile(fileName("-test-02"))
  private def testData3 = parseFile(fileName("-test-03"))
  private def testData4 = parseFile(fileName("-test-04"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "test 0" in {
      part1(testData0) shouldEqual 140
    }

    "test 1" in {
      part1(testData1) shouldEqual 772
    }

    "test 2" in {
      part1(testData2) shouldEqual 1930
    }

    "real" in {
      part1(realData) shouldEqual 1371306
    }
  }

  "part 2" - {
    "test 0" in {
      part2(testData0) shouldEqual 80
    }

    "test 1" in {
      part2(testData1) shouldEqual 436
    }

    "test 3" in {
      part2(testData3) shouldEqual 236
    }

    "test 4" in {
      part2(testData4) shouldEqual 368
    }

    "test 2" in {
      part2(testData2) shouldEqual 1206
    }

    "real" in {
      part2(realData) shouldEqual 805880
    }
  }
}
