package jurisk.adventofcode.y2024

import Advent17._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent17Spec extends AnyFreeSpec {
  "part 1" - {
    "test 0" in {
      part1(State(729, 0, 0, List(0, 1, 5, 4, 3, 0))) shouldEqual "4,6,3,5,6,3,5,2,1,0"
    }

    "test 1" in {
      State(0, 0, 9, List(2, 6)).step.fold(identity, identity).b shouldEqual 1
    }

    "real" in {
      part1(RealData) shouldEqual "1,5,0,3,7,3,0,3,1"
    }
  }

  "part 2" - {
//    "test" in {
//      part2(State(2024, 0, 0, List(0,3,5,4,3,0))) shouldEqual 117440
//    }

    "test" in {
      val expected = Array(1,5,0,3,7,3,0,3,1)

      getOutput(RealData) shouldEqual expected.toList
      runFormula(RealData.a.toInt) shouldEqual expected.map(_.toLong).toVector

      getOutput(RealData.copy(a = 777)) shouldEqual List(2, 4, 1, 2)
      runFormula(777) shouldEqual List(2, 4, 1, 2)
      getOutput(RealData.copy(a = 137505178)) shouldEqual List(2, 4, 1, 5, 7, 5, 1, 6, 3, 2)
      runFormula(137505178) shouldEqual List(2, 4, 1, 5, 7, 5, 1, 6, 3, 2)

      getOutput(RealData.copy(a = 2203)) shouldEqual List(2L, 4, 1, 5)
      runFormula(2203) shouldEqual List(2L, 4, 1, 5)

      Advent17.solver(List(2L, 4, 1, 5).map(_.toInt)) shouldEqual 2203

      Advent17.solver(RealData.program) shouldEqual 105981155568026L
//
//      State(44374556, 0, 0, expected.toList)
//
//      Advent17.run(44374555L, expected) shouldEqual false
//      Advent17.run(44374556L, expected) shouldEqual true
//      Advent17.run(44374557L, expected) shouldEqual false
//
//      Advent17.runFormula(44374556) shouldEqual expected.map(_.toLong).toVector
    }

    "test 2" in {
      val expected = Array(2, 4, 1, 5)

      Advent17.run(2203, expected) shouldEqual true
    }

    "real" in {
      part2(RealData) shouldEqual 105981155568026L
    }
  }
}
