package jurisk.adventofcode

import Advent00._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent00Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2023/02-test.txt")) shouldEqual 5 + 1234567
    }

    "real" in {
      part1(parseFile("2023/02.txt")) shouldEqual 100 + 1234567
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/02-test.txt")) shouldEqual 5 + 1234567
    }

    "real" in {
      part2(parseFile("2023/02.txt")) shouldEqual 100 + 1234567
    }
  }
}
