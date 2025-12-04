package jurisk.adventofcode.y2025

import cats.implicits.catsSyntaxOptionId
import jurisk.adventofcode.y2025.Advent03._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent03Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))

  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 98 + 89 + 78 + 92
    }

    "test 811111111111119" in {
      jolts(
        List(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9),
        2,
      ) shouldEqual 89.some
    }

    "test 81" in {
      jolts(List(8, 1), 1) shouldEqual 8.some
    }

    "real" in {
      part1(realData) shouldEqual 17113
    }
  }

  "part 2" - {
    "test" in {
      part2(testData) shouldEqual 3121910778619L
    }

    "test 234234234234278" in {
      jolts(
        List(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8),
        12,
      ) shouldEqual 434234234278L.some
    }

    "real" in {
      part2(realData) shouldEqual 169709990062889L
    }
  }
}
