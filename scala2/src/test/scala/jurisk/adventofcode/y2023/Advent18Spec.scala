package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent18._
import org.scalatest.matchers.should.Matchers._

class Advent18Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 62
    }

    "real" in {
      part1(realData) shouldEqual 106459
    }
  }

  "apply picks shoelace to part 1 data" - {
    "test" in {
      solvePicksShoelace(testData.map(_.part1)) shouldEqual 62
    }

    "real" in {
      solvePicksShoelace(realData.map(_.part1)) shouldEqual 106459
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 952408144115L
    }

    "real" in {
      part2(realData) shouldEqual 63806916814808L
    }
  }
}
