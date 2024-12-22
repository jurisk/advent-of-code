package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventAppSpec
import jurisk.adventofcode.y2020.Advent01.{solution1, solution2}
import org.scalatest.matchers.should.Matchers.*

class Advent01Spec extends AdventAppSpec(Advent01):
  private val testData = List(1721, 979, 366, 299, 675, 1456)

  "part1" - {
    "test" in {
      solution1(testData) shouldEqual 1721 * 299
    }

    "real" in {
      solution1(realData) shouldEqual 1016619
    }
  }

  "part2" - {
    "test" in {
      solution2(testData) shouldEqual 241861950
    }

    "real" in {
      solution2(realData) shouldEqual 218767230
    }
  }
