package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent14._

class Advent14Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData, 11, 7) shouldEqual 12
    }

    "real" in {
      part1(realData, 101, 103) shouldEqual 221655456
    }
  }

  "part 2" - {
    "real" ignore {
      part2(realData, 101, 103) shouldEqual 7858
    }
  }
}
