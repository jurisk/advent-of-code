package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent24._
import cats.implicits._
import org.scalatest.matchers.should.Matchers._

class Advent24Spec extends AnyFreeSpec {
  // https://www.math3d.org/1XG5CNDSK
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  val expectedTestAnswer: PositionAndVelocity3D = PositionAndVelocity3D(
    Coordinates3D(24, 13, 10),
    Coordinates3D(-3, 1, 2),
  )

  "part 1" - {
    "test" in {
      part1(testData, 7, 27) shouldEqual 2
    }

    "real" in {
      // Not 24198
      part1(realData, 200000000000000L, 400000000000000L) shouldEqual 24192
    }
  }

  // https://www.mathsisfun.com/algebra/vectors-cross-product.html
  "crossProduct" - {
    crossProduct(
      Coordinates3D(2, 3, 4),
      Coordinates3D(5, 6, 7),
    ) shouldEqual Coordinates3D(
      -3,
      6,
      -3,
    )
  }

  "areVectorsParallel" in {
    val a = Coordinates3D(5, 2, -1)
    val b = Coordinates3D(-10, -4, 2)
    areVectorsParallel(a, b) shouldEqual true
    areVectorsParallel(a, expectedTestAnswer.velocity) shouldEqual false
  }

  "linesIntersect" in {
    testData foreach { rock =>
      linesIntersect(rock, expectedTestAnswer) shouldEqual true
//      linesIntersect(rock, PositionAndVelocity3D(Coordinates3D(0, 0, 0), Coordinates3D(0, 0, 0))) shouldEqual false
      linesIntersect(
        rock,
        PositionAndVelocity3D(Coordinates3D(-3, -4, -1), Coordinates3D(1, 2, 3)),
      ) shouldEqual false
    }
  }

  "lineIntersection3D" - {
    // https://emedia.rmit.edu.au/learninglab/content/v7-intersecting-lines-3d
    "from webpage" - {
      "1" in {
        lineIntersection3D(
          PositionAndVelocity3D(
            Coordinates3D(1, -4, 8),
            Coordinates3D(2, 1, -2),
          ),
          PositionAndVelocity3D(
            Coordinates3D(5, 1, 8),
            Coordinates3D(1, -1, -3),
          ),
        ) shouldEqual (3, 2, Coordinates3D(7, -1, 2)).some
      }

      "2" in {
        lineIntersection3D(
          PositionAndVelocity3D(
            Coordinates3D(0, -4, 8),
            Coordinates3D(1, 1, -2),
          ),
          PositionAndVelocity3D(
            Coordinates3D(2, -1, 3),
            Coordinates3D(2, 1, -2),
          ),
        ) shouldEqual none
      }
    }

    "first rock" in {
      lineIntersection3D(
        expectedTestAnswer,
        PositionAndVelocity3D(
          Coordinates3D(19, 13, 30),
          Coordinates3D(-2, 1, -2),
        ),
      ) shouldEqual (5, 1234, Coordinates3D(9, 18, 20)).some
    }

    "all test rocks" in {
      testData foreach { rock =>
        lineIntersection3D(expectedTestAnswer, rock).isDefined shouldEqual true
      }
    }
  }

  "part 2" - {
    "test" in {
      solvePart2(testData).position shouldEqual expectedTestAnswer.position
      part2(testData) shouldEqual 47
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
