package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent20._
import jurisk.adventofcode.y2023.Advent20.Pulse.{High, Low}
import org.scalatest.matchers.should.Matchers._

class Advent20Spec extends AnyFreeSpec {
  private def testData1 = parseFile(fileName("-test-1"))
  private def testData2 = parseFile(fileName("-test-2"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "simple" in {
      val result = solve1(testData1, 1)
      result.pulsesSent shouldEqual Map(Low -> 8, High -> 4)
    }

    "test 1" in {
      part1(testData1) shouldEqual 32000000
    }

    "test 2" in {
      part1(testData2) shouldEqual 11687500
    }

    "real" in {
      part1(realData) shouldEqual 788081152
    }
  }

  "part 2" - {
    "real" in {
      part2(realData) shouldEqual 224602011344203L
    }
  }
}
