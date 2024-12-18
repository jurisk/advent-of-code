package jurisk.adventofcode.y2024

import Advent18._
import jurisk.geometry.Coords2D
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent18Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  private val TestEnd = Coords2D(6, 6)

  "part 1" - {
    "test" in {
      part1(testData, 12, Coords2D.Zero, TestEnd) shouldEqual 22
    }

    "real" in {
      part1(realData, 1024, Coords2D.Zero, RealEnd) shouldEqual 318
    }
  }

  "part 2" - {
    "test" in {
      part2(testData, Coords2D.Zero, TestEnd) shouldEqual "6,1"
    }

    "real" ignore {
      part2(realData, Coords2D.Zero, RealEnd) shouldEqual "56,29"
    }
  }
}
