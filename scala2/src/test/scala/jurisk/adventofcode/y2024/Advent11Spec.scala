package jurisk.adventofcode.y2024

import Advent11._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent11Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      blink(Vector(0, 1, 10, 99, 999)) shouldEqual Vector(1, 2024, 1, 0, 9, 9,
        2021976)
      blink(Vector(125, 17)) shouldEqual Vector(253000, 1, 7)
      blinkNTimes(Vector(125, 17), 2) shouldEqual Vector(253, 0, 2024, 14168)
      part1(Vector(125, 17), 6) shouldEqual 22
      part1(Vector(125, 17)) shouldEqual 55312
    }

    "real" in {
      part1(realData) shouldEqual 184927
    }
  }

  "part 2" - {
    "real 25" in {
      part2(realData, 25) shouldEqual 184927
    }
    "real 75" in {
      part2(realData) shouldEqual 220357186726677L
    }
  }
}
