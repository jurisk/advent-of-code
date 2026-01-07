package jurisk.adventofcode.y2025

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import jurisk.adventofcode.y2025.Advent12._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent12Spec extends AsyncFreeSpec with AsyncIOSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "individual regions" - {
    "4x4: 0 0 0 0 2 0 - should be valid" in {
      val data   = testData
      val region = data.regions(0)
      region.isValid(data.shapes).isRight shouldEqual true
    }

    "12x5: 1 0 1 0 2 2 - should be valid" in {
      val data   = testData
      val region = data.regions(1)
      region.isValid(data.shapes).isRight shouldEqual true
    }

    "12x5: 1 0 1 0 3 2 - should have no solution" ignore {
      val data   = testData
      val region = data.regions(2)
      region.isValid(data.shapes).isLeft shouldEqual true
    }
  }

  "part 1" - {
    "test" ignore {
      part1[IO](testData) asserting { _ shouldEqual 2 }
    }

    "real" ignore {
      part1[IO](realData) asserting { _ shouldEqual 463 }
    }
  }
}
