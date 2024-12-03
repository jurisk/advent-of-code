package jurisk.adventofcode.y2024

import Advent03._
import jurisk.utils.FileInput.readFileText
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent03Spec extends AnyFreeSpec {
  private def testData00 = readFileText(fileName("-test-00"))
  private def testData01 = readFileText(fileName("-test-01"))
  private def realData   = readFileText(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData00) shouldEqual 2 * 4 + 5 * 5 + 11 * 8 + 8 * 5
    }

    "real" in {
      part1(realData) shouldEqual 187825547
    }
  }

  "part 2" - {
    "test" in {
      part2(testData01) shouldEqual 2 * 4 + 8 * 5
    }

    "real" in {
      part2(realData) shouldEqual 85508223
    }
  }
}
