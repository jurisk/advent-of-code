package jurisk.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent09._

class Advent09Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 1928
    }

    "real" in {
      part1(realData) shouldEqual 6471961544878L
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 2858
    }

    "real" in {
      part2(realData) shouldEqual 6511178035564L
    }
  }
}
