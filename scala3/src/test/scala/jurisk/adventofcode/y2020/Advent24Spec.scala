package jurisk.adventofcode.y2020

import org.scalatest.freespec.AsyncFreeSpec
import Advent24.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers.*

class Advent24Spec extends AsyncFreeSpec with AsyncIOSpec:
  private val testData = parseTestData("00").unsafeRunSync().getOrElse(sys.error("failed"))
  private val realData = parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))

  "solution1" - {
    "test" in {
      solution1(testData) shouldEqual 10
    }
    
    "real" in {
      solution1(realData) shouldEqual 275
    }
  }

  "solution2" - {
    "test" in {
      solution2(testData) shouldEqual 2208
    }
    
    "real" in {
      solution2(realData) shouldEqual 3537
    }
  }

