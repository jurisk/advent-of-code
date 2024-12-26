package jurisk.adventofcode.y2024

import Advent24._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent24Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 2024
    }

    "real" in {
      part1(realData) shouldEqual 58740594706150L
    }
  }

  "part 2" - {
    "real" ignore {
      part2(realData) shouldEqual "cvh,dbb,hbk,kvn,tfn,z14,z18,z23"
    }
  }
}
