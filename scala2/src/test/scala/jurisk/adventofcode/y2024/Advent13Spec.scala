package jurisk.adventofcode.y2024

import Advent13._
import jurisk.adventofcode.y2024.Advent13.SolutionMode.{ExternalZ3, InternalZ3}
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
      val example  = testData.head
      val expected = Some(80 * 3 + 40)
      example.solve(InternalZ3) shouldEqual expected
      example.solve(ExternalZ3) shouldEqual expected
    }

    "real" in {
      part2(realData) shouldEqual 108528956728655L
    }
  }
}
