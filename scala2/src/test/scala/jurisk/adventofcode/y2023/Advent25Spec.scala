package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import Advent25._

class Advent25Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 54
    }

    "real" ignore {
      part1(realData) shouldEqual 582626
    }
  }
}
