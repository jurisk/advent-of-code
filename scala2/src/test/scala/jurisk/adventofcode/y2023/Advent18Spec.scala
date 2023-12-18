package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent18._
import org.scalatest.matchers.should.Matchers._

class Advent18Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      // TODO: actually fails, off by one !?
      part1(testData) shouldEqual 62
    }

    "real" in {
      // not 106460
      // not 106459
      part1(realData) shouldEqual 106459
    }
  }

  "part 2" - {

    "asdf" in {
      solve(testData) shouldEqual 62
    }

    "qq" in {
      solve(realData) shouldEqual 106459
    }

    "test" in {
      part2(testData) shouldEqual 952408144115L
    }

    "real" in {
      part2(realData) shouldEqual 63806916814808L
    }
  }
}
