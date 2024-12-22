package jurisk.adventofcode.y2020

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers.*
import Advent20.*

class Advent20Spec extends AsyncFreeSpec with AsyncIOSpec:
  private val testData = parseTestData("00").unsafeRunSync().getOrElse(sys.error("failed"))
  private val realData = parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))

  "solution1" - {
    "test" in {
      solution1(testData) shouldEqual 20899048083289L
    }

    "real" in {
      solution1(realData) shouldEqual 60145080587029L
    }
  }

  "solution2" - {
    "test" in {
      solution2(testData) shouldEqual 273
    }

    "real" in {
      solution2(realData) shouldEqual 1901
    }
  }
