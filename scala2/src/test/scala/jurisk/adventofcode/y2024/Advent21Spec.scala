package jurisk.adventofcode.y2024

import Advent21._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "overall" - {
    "toPressNumericButton" in {
      toPressNumericButton(
        NumericButton.Activate,
        NumericButton.Zero,
      ) shouldEqual
        Set(DirectionalButton.parseList("<A"))

      toPressNumericButton(
        NumericButton.Zero,
        NumericButton.Two,
      ) shouldEqual
        Set(DirectionalButton.parseList("^A"))

      toPressNumericButton(
        NumericButton.Two,
        NumericButton.Nine,
      ) shouldEqual
        Set(
          DirectionalButton.parseList("^^>A"),
          DirectionalButton.parseList(">^^A"),
        )
    }

    "firstLevelPresses" in {
      Code("029A").firstLevelPresses() shouldEqual Set(
        DirectionalButton.parseList("<A^A>^^AvvvA"),
//      Deliberately skipping from example as it is suboptimal: DirectionalButton.parseList("<A^A^>^AvvvA"),
        DirectionalButton.parseList("<A^A^^>AvvvA"),
      )
    }
  }

  "part 1" - {
    "test 029A" in {
      val code = Code("029A")
      code.humanPresses.length shouldEqual 68
      code.numericPart shouldEqual 29
      code.complexity shouldEqual 68 * 29
    }

    "test" in {
      part1(testData) shouldEqual 126384
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
