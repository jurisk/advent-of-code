package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent25._

class Advent25Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      solve(testData) shouldEqual 54
    }

    "real" in {
      solve(realData) shouldEqual 582626
    }
  }
}
