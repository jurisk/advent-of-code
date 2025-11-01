package jurisk.adventofcode.y2018

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent17._

class Advent17Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(simulate(parseFile("2018/17-test.txt"))) shouldEqual 57
    }

    "real" ignore {
      part1(simulate(parseFile("2018/17.txt"))) shouldEqual 33004
    }
  }

  "part 2" - {
    "test" in {
      part2(simulate(parseFile("2018/17-test.txt"))) shouldEqual 29
    }

    "real" ignore {
      part2(simulate(parseFile("2018/17.txt"))) shouldEqual 23294
    }
  }
}
