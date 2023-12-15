package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent15._
import org.scalatest.matchers.should.Matchers._

class Advent15Spec extends AnyFreeSpec {
  "calculateHash" - {
    "HASH" in {
      calculateHash("HASH") shouldEqual 52
    }

    "rn=1" in {
      calculateHash("rn=1") shouldEqual 30
    }
  }

  "part 1" - {
    "test" in {
      part1(parseFile("2023/15-test.txt")) shouldEqual 1320
    }

    "real" in {
      part1(parseFile("2023/15.txt")) shouldEqual 521341
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/15-test.txt")) shouldEqual 145
    }

    "real" in {
      part2(parseFile("2023/15.txt")) shouldEqual 252782
    }
  }
}
