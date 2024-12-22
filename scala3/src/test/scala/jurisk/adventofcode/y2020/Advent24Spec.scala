package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventAppSpec
import jurisk.adventofcode.y2020.Advent24.*
import org.scalatest.matchers.should.Matchers.*

class Advent24Spec extends AdventAppSpec(Advent24):
  "solution1" - {
    "test" in {
      solution1(testData00) shouldEqual 10
    }

    "real" in {
      solution1(realData) shouldEqual 275
    }
  }

  "solution2" - {
    "test" in {
      solution2(testData00) shouldEqual 2208
    }

    "real" in {
      solution2(realData) shouldEqual 3537
    }
  }

