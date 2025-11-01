package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent23._

class Advent23Spec extends AnyFreeSpec {
  private def testData   = parseFile(fileName("-test"))
  private def simpleData = parseFile(fileName("-test-simple"))
  private def realData   = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 94
    }

    "real" in {
      part1(realData) shouldEqual 1966
    }
  }

  "part 2" - {
    "solve2BacktrackingUsingBacktracker1 " in {
      val converted            = convertPart1ToPart2(testData)
      val (start, graph, goal) = inputToGraph(converted)
      solve2BacktrackingUsingBacktracker(start, graph, goal) shouldEqual 154
    }

    "solve2BacktrackingUsingBacktracker 2" in {
      val converted            = convertPart1ToPart2(testData)
      val (start, graph, goal) = convertToSimplified(converted)
      solve2BacktrackingUsingBacktracker(start, graph, goal) shouldEqual 154
    }

    "simple test using part 1" in {
      part2UsingPart1(simpleData) shouldEqual part2(simpleData)
    }

    "test using part 1" in {
      part2UsingPart1(testData) shouldEqual 154
    }

    "test" in {
      part2(testData) shouldEqual 154
    }

    "real" in {
      part2(realData) shouldEqual 6286
    }
  }
}
