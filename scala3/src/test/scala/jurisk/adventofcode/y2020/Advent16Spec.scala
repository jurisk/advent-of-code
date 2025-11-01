package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventAppSpec
import jurisk.adventofcode.y2020.Advent16._
import org.scalatest.matchers.should.Matchers._

class Advent16Spec extends AdventAppSpec(Advent16):
  "solution1" - {
    "test" in {
      solution1(testData00) shouldEqual 71
    }

    "real" in {
      solution1(realData) shouldEqual 21980
    }
  }

  "solution2" - {
    "test" in {
      solve2(testData01, "class") shouldEqual 12
      solve2(testData01, "row") shouldEqual 11
      solve2(testData01, "seat") shouldEqual 13
    }

    "real" in {
      solution2(realData) shouldEqual 1439429522627L
    }
  }
