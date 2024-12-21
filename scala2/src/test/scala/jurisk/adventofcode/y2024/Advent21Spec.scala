package jurisk.adventofcode.y2024

import Advent21._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "utility" - {
    "toPressNumericButton" in {
      (NumericButton.Activate pathTo NumericButton.Zero) shouldEqual Set(
        DirectionalButton.parseList("<A")
      )
      (NumericButton.Zero pathTo NumericButton.Two) shouldEqual Set(
        DirectionalButton.parseList("^A")
      )
      (NumericButton.Two pathTo NumericButton.Nine) shouldEqual Set(
        DirectionalButton.parseList("^^>A"),
        DirectionalButton.parseList(">^^A"),
      )
    }
  }

  "part 1" - {
    "test 029A" in {
      val code = Code("029A")
      code.bestHumanPressesLength(2) shouldEqual 68
      code.numericPart shouldEqual 29
    }

    "test" in {
      part1(testData) shouldEqual 126384
    }

    "real" in {
      part1(realData) shouldEqual 270084
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 154115708116294L
    }

    "real" in {
      part2(realData) shouldEqual 329431019997766L
    }
  }
}
