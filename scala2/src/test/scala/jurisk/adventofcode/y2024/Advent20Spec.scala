package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent20._

class Advent20Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test 64" in {
      part1(testData, 64) shouldEqual 1
    }

    "test 40" in {
      part1(testData, 40) shouldEqual 1 + 1
    }

    "test misc" in {
      part1(testData, 38) shouldEqual 1 + 1 + 1
      part1(testData, 36) shouldEqual 1 + 1 + 1 + 1
      part1(testData, 20) shouldEqual 1 + 1 + 1 + 1 + 1
      part1(testData, 12) shouldEqual 1 + 1 + 1 + 1 + 1 + 3
      part1(testData, 10) shouldEqual 1 + 1 + 1 + 1 + 1 + 3 + 2
      part1(testData, 8) shouldEqual 1 + 1 + 1 + 1 + 1 + 3 + 2 + 4
      part1(testData, 6) shouldEqual 1 + 1 + 1 + 1 + 1 + 3 + 2 + 4 + 2
      part1(testData, 4) shouldEqual 1 + 1 + 1 + 1 + 1 + 3 + 2 + 4 + 2 + 14
      part1(testData, 2) shouldEqual 1 + 1 + 1 + 1 + 1 + 3 + 2 + 4 + 2 + 14 + 14
    }

    "real" in {
      part1(realData, 100) shouldEqual 1293
    }
  }

  "part 2" - {
    "test" in {
      part2(testData, 76) shouldEqual 3
    }

    "real" in {
      part2(realData, 100) shouldEqual 977747
    }
  }
}
