package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent21._
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData, 6) shouldEqual 16
    }

    "real" in {
      part1(realData, 64) shouldEqual 3816
    }
  }

  "part 2" - {
    "test 6" in {
      part2(testData, 6) shouldEqual 16
    }

    "test 10" in {
      part2(testData, 10) shouldEqual 50
    }

    "test 50" in {
      part2(testData, 50) shouldEqual 1594
    }

    "test 100" in {
      part2(testData, 100) shouldEqual 6536
    }

    "test 500" in {
      part2(testData, 500) shouldEqual 167004
    }

    "test 1000" in {
      part2(testData, 1000) shouldEqual 668697
    }

    "test 5000" in {
      part2(testData, 5000) shouldEqual 16733044
    }

    "real" in {
      part2(realData, 26501365) shouldEqual 0
    }
  }
}
