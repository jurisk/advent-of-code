package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent16._
import cats.implicits._
import jurisk.geometry.{Coords2D, Direction2D}
import org.scalatest.matchers.should.Matchers._

class Advent16Spec extends AnyFreeSpec {
  private val testData: Input = parseFile("2023/16-test.txt")
  private val realData: Input = parseFile("2023/16.txt")

  "solveByOptimization - simple maximization test that fails due to a loop" ignore {
    val test = parse("""/-
                       |/|
                       |\/
                       |""".stripMargin)

    solveByOptimization(
      test,
      (Coords2D.Zero, Direction2D.W).some,
      MinimizeOrMaximize.Maximize,
      debug = true,
    ) shouldEqual 1
  }

  "part 1" - {
    val TestAnswer = 46
    val RealAnswer = 7415

    "simulate" - {
      "test" in {
        part1Simulation(testData) shouldEqual TestAnswer
      }

      "real" in {
        part1Simulation(realData) shouldEqual RealAnswer
      }
    }

    "optimize" - {
      "test" in {
        part1Optimization(testData) shouldEqual TestAnswer
      }

      "real" in {
        part1Optimization(realData) shouldEqual RealAnswer
      }
    }
  }

  "part 2" - {
    val TestAnswer = 51
    val RealAnswer = 7943

    "simulate" - {
      "test" in {
        part2Simulation(testData) shouldEqual TestAnswer
      }

      "real" in {
        part2Simulation(realData) shouldEqual RealAnswer
      }
    }

    "optimize" - {
      // One-pass Part 2 using optimization doesn't work (see other comments and the failing test)

      "test" ignore {
        part2Optimization(testData) shouldEqual TestAnswer
      }

      "real" ignore {
        part2Optimization(realData) shouldEqual RealAnswer
      }
    }
  }
}
