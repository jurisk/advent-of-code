package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent22._
import org.scalatest.matchers.should.Matchers._

class Advent22Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 5
    }

    "real" ignore {
      part1(realData) shouldEqual 512
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 7
    }

    "real" ignore {
      part2(realData) shouldEqual 98167
    }
  }
}
