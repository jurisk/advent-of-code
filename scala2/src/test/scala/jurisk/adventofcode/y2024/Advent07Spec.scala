package jurisk.adventofcode.y2024

import Advent07._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent07Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 3749
    }

    "3267: 81 40 27" in {
      val equation = Equation.parse("3267: 81 40 27")
      validate(
        allowPipe = false,
        equation.result,
        equation.numbers,
      ) shouldBe true
    }

    "real" in {
      part1(realData) shouldEqual 1038838357795L
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 11387
    }

    "real" in {
      part2(realData) shouldEqual 254136560217241L
    }
  }
}
