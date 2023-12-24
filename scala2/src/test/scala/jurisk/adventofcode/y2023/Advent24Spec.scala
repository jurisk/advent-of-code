package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent24._
import jurisk.geometry.{Area2D, Coords2D}
import org.scalatest.matchers.should.Matchers._

class Advent24Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData, 7, 27) shouldEqual 2
    }

    "real" in {
      // Not 24198
      part1(realData, 200000000000000L, 400000000000000L) shouldEqual 24192
    }
  }

  "part 2" - {
    "test" in {
      val valid = PositionAndVelocity3D(
        Coordinates3D(24, 13, 10),
        Coordinates3D(-3, 1, 2),
      )
      solvePart2(testData) shouldEqual valid
      part2(testData) shouldEqual 47
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
