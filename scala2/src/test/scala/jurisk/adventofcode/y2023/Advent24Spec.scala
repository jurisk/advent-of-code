package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent24._
import jurisk.geometry.Coords3D
import org.scalatest.matchers.should.Matchers._

class Advent24Spec extends AnyFreeSpec {
  // https://www.math3d.org/G5JHCaIly
  private def testData = parseFile(fileName("-test"))

  private def realData = parseFile(fileName(""))

  val expectedTestAnswer: PositionAndVelocity3D = PositionAndVelocity3D(
    Coords3D[Long](24, 13, 10),
    Coords3D[Long](-3, 1, 2),
  )

  val expectedRealAnswer: PositionAndVelocity3D = PositionAndVelocity3D(
    Coords3D[Long](191146615936494L, 342596108503183L, 131079628110881L),
    Coords3D[Long](139, -93, 245),
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
    (Coords3D[Long](2, 3, 4) crossProduct Coords3D[Long](
      5,
      6,
      7,
    )) shouldEqual Coords3D[Long](
      -3,
      6,
      -3,
    )
  }

  "areVectorsParallel" in {
    val a = Coords3D[Long](5, 2, -1)
    val b = Coords3D[Long](-10, -4, 2)
    areVectorsParallel(a, b) shouldEqual true
    areVectorsParallel(a, expectedTestAnswer.velocity) shouldEqual false
  }

  "linesIntersect" in {
    testData foreach { rock =>
      linesIntersect(rock, expectedTestAnswer) shouldEqual true
      linesIntersect(
        rock,
        PositionAndVelocity3D(
          Coords3D[Long](-3, -4, -1),
          Coords3D[Long](1, 2, 3),
        ),
      ) shouldEqual false
    }
  }

  // TODO:  The embedded `z3-turnkey` seems to be broken as it is much slower than command line `z3`.
  //        Switch to the command line one.
  "part 2 Z3" - {
    "test optimizer" ignore {
      solvePart2Optimizer(testData) shouldEqual expectedTestAnswer
    }

    "real optimizer" ignore {
      solvePart2Optimizer(realData) shouldEqual expectedRealAnswer
    }
  }

  "part 2" - {
    val expectedResult = 664822352550558L

    "real inferring velocity" in {
      solve2InferringVelocity(realData) shouldEqual expectedRealAnswer
    }

    "real using CRT" ignore {
      // Not actually implemented
      solve2UsingChineseRemainderTheorem(realData) shouldEqual expectedResult
    }

    "real" in {
      part2(realData) shouldEqual expectedResult
    }
  }
}
