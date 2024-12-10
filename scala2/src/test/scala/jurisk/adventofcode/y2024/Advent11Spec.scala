package jurisk.adventofcode.y2024

import Advent11._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent11Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 0
    }

    "real" in {
      part1(realData) shouldEqual 0
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
