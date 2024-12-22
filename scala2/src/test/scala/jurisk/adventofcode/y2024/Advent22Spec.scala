package jurisk.adventofcode.y2024

import Advent22._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent22Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "next" in {
      val a = next(123)
      a shouldEqual 15887950
    }

    "test 1" in {
      nthSecretNumber(1, 2000) shouldEqual 8685429
    }

    "test" in {
      part1(testData0) shouldEqual 37327623
    }

    "real" in {
      part1(realData) shouldEqual 13022553808L
    }
  }

  "part 2" - {
    "test" in {
      part2(testData1) shouldEqual 7 + 7 + 9
    }

    "real" in {
      part2(realData) shouldEqual 0
    }
  }
}
