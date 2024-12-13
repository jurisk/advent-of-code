package jurisk.adventofcode.y2024

import Advent13._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent13Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 480
    }

    "real" in {
      part1(realData) shouldEqual 25751
    }
  }

  "part 2" - {
    "test #0" in {
      val example2 = testData.head
      example2.solve2 shouldEqual Some(80 * 3 + 40)
    }

    "real" in {
      part2(realData) shouldEqual 108528956728655L
    }
  }
}
