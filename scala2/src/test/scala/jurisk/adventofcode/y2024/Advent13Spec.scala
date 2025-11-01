package jurisk.adventofcode.y2024

import jurisk.adventofcode.y2024.Advent13.SolutionMode.LinearEquations
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3Mode.External
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3Mode.Internal
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent13._

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
    "test #0 Z3" ignore {
      val example  = testData.head
      val expected = Some(80 * 3 + 40)
      example.solve(Z3(Internal)) shouldEqual expected
      example.solve(Z3(External)) shouldEqual expected
    }

    "test #0 linear equations" in {
      val example  = testData.head
      val expected = Some(80 * 3 + 40)
      example.solve(LinearEquations) shouldEqual expected
    }

    "real" in {
      part2(realData) shouldEqual 108528956728655L
    }
  }
}
