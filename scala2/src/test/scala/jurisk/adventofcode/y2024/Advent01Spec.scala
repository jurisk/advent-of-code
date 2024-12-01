package jurisk.adventofcode.y2024

import Advent01._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent01Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      Advent01.part1(testData) shouldEqual 11
    }

    "real" in {
      part1(realData) shouldEqual 1530215
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 31
    }

    "real" in {
      part2(realData) shouldEqual 26800609
    }
  }
}
