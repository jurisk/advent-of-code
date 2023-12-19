package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent19._
import org.scalatest.matchers.should.Matchers._

class Advent19Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 19114
    }

    "real" in {
      part1(realData) shouldEqual 377025
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 0
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
