package jurisk.adventofcode.y2020

import org.scalatest.matchers.should.Matchers.*
import Advent20.*
import jurisk.adventofcode.AdventAppSpec

class Advent20Spec extends AdventAppSpec(Advent20):
  "solution1" - {
    "test" in {
      solution1(testData00) shouldEqual 20899048083289L
    }

    "real" in {
      solution1(realData) shouldEqual 60145080587029L
    }
  }

  "solution2" - {
    "test" in {
      solution2(testData00) shouldEqual 273
    }

    "real" in {
      solution2(realData) shouldEqual 1901
    }
  }
