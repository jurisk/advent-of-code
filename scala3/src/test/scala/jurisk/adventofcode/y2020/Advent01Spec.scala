package jurisk.adventofcode.y2020

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers.*

class Advent01Spec extends AsyncFreeSpec with AsyncIOSpec:
  private val testData = List(1721, 979, 366, 299, 675, 1456)
  private val realData = Advent01.parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))

  "part1" - {
    "test" in {
      Advent01.solution1(testData) shouldEqual 1721 * 299
    }

    "real" in {
      Advent01.solution1(realData) shouldEqual 1016619
    }
  }

  "part2" - {
    "test" in {
      Advent01.solution2(testData) shouldEqual 241861950
    }

    "real" in {
      Advent01.solution2(realData) shouldEqual 218767230
    }
  }