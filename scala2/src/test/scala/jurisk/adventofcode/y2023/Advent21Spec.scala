package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent21._
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData, 6) shouldEqual 16
    }

    "real" in {
      part1(realData, 64) shouldEqual 3816
    }
  }

  "nextCounts" - {
    val test = parse(
      s""".S
         |##
         |""".stripMargin)

    "simple 1" in {
      part2(test, 1) shouldEqual 2
    }

    "simple 2" in {
      part2(test, 2) shouldEqual 3
    }

//
//    "simple zoom" in {
//      (1 to 10) foreach { n =>
//        val a = part2(test, n)
//        val b = part2Old(test, n)
//
//        println(n)
//        a shouldEqual b
//      }
//    }

  }

  "part 2" - {
    "test 6" in {
      part2(testData, 6) shouldEqual 16
    }

    "test 7" in {
      part2(testData, 7) shouldEqual 22
    }

    "test 8" in {
      part2(testData, 8) shouldEqual 30
    }

    "test 10" in {
      part2(testData, 10) shouldEqual 50
    }

    "test 50" in {
      part2(testData, 50) shouldEqual 1594
    }

    "test 100" in {
      part2(testData, 100) shouldEqual 6536
    }

    "test 500" in {
      part2(testData, 500) shouldEqual 167004
    }

    "test 1000" in {
      part2(testData, 1000) shouldEqual 668697
    }

    "test 5000" in {
      part2(testData, 5000) shouldEqual 16733044
    }

    "real" in {
      part2(realData, 26501365) shouldEqual 0
    }
  }
}
