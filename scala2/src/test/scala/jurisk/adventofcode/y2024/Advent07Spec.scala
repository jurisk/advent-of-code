package jurisk.adventofcode.y2024

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent07._

class Advent07Spec extends AsyncFreeSpec with AsyncIOSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) asserting {
        _ shouldEqual 3749
      }
    }

    "3267: 81 40 27" in {
      val equation = Equation.parse("3267: 81 40 27")
      f(
        allowPipe = false,
        equation.result,
        equation.numbers,
      ) shouldBe true
    }

    "real" in {
      part1(realData) asserting {
        _ shouldEqual 1038838357795L
      }
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) asserting {
        _ shouldEqual 11387
      }
    }

    "real" in {
      part2(realData) asserting {
        _ shouldEqual 254136560217241L
      }
    }
  }
}
