package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent21._
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData       = parseFile(fileName("-test"))
  private def realData       = parseFile(fileName(""))
  private def testEmpty      = parseFile(fileName("-test-empty"))
  private def testSingleRock = parseFile(fileName("-test-single-rock"))
  private def testSimpleWall = parseFile(fileName("-test-simple-wall"))
//
//  "part 1" - {
//    "test" in {
//      part1(testData, 6) shouldEqual 16
//    }
//
//    "test 255" in {
//      part1(testData, 255) shouldEqual 39
//    }
//
//    "test 256" in {
//      part1(testData, 256) shouldEqual 42
//    }
//
//    "test 257" in {
//      part1(testData, 257) shouldEqual 39
//    }
//
//    "test 258" in {
//      part1(testData, 258) shouldEqual 42
//    }
//
//    "real" in {
//      part1(realData, 64) shouldEqual 3816
//    }
//
//    "real 255" in {
//      part1(realData, 255) shouldEqual 7748
//    }
//
//    "real 256" in {
//      part1(realData, 256) shouldEqual 7757
//    }
//
//    "real 257" in {
//      part1(realData, 257) shouldEqual 7748
//    }
//
//    "real 258" in {
//      part1(realData, 258) shouldEqual 7757
//    }
//
//  }

//  "nextCounts" - {
//    val test = parse(s""".S
//                        |##
//                        |""".stripMargin)
//
//    "simple 1" ignore {
//      part2(test, 1) shouldEqual 2
//    }
//
//    "simple 2" ignore {
//      part2(test, 2) shouldEqual 3
//    }
//
//    "simple zoom" ignore {
//      (1 to 10) foreach { n =>
//        val a = part2(test, n)
//        val b = part2Old(test, n)
//
//        println(n)
//        a shouldEqual b
//      }
//    }
//
//  }

  "FieldCounts" - {
    "3" - {
      val size        = 3
      val emptyCounts = InnerCounts(Map.empty, 0)

      "0" in {
        calculateFieldCounts(0, size) shouldEqual FieldCounts(
          edgeCenter = emptyCounts,
          corner = emptyCounts,
        )
      }

      "1" in {
        calculateFieldCounts(1, size) shouldEqual FieldCounts(
          edgeCenter = emptyCounts,
          corner = emptyCounts,
        )
      }

      "2" in {
        calculateFieldCounts(2, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(1L -> 1L), 0),
          corner = emptyCounts,
        )
      }

      "3" in {
        calculateFieldCounts(3, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 0),
          corner = emptyCounts,
        )
      }

      "4" in {
        calculateFieldCounts(4, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(3L -> 1L), 0),
          corner = InnerCounts(Map(1L -> 1L), 0),
        )
      }

      "5" in {
        calculateFieldCounts(5, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(1L -> 1L), 1),
          corner = InnerCounts(Map(2L -> 1L), 0),
        )
      }

      "6" in {
        calculateFieldCounts(6, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 1),
          corner = InnerCounts(Map(3L -> 1L), 0),
        )
      }
    }
  }

  "comparison" - {
    Map(
      "empty"       -> testEmpty,
      "single-rock" -> testSingleRock,
      "simple-wall" -> testSimpleWall,
    ) map { case (name, test) =>
      name - {
        val MaxN = 20 // TODO: 100?
        1 to MaxN map { n =>
          s"$name - $n" in {
            val old                     = part2Old(test, n)
            val distFieldClassification = part2FieldClassification(test, n)
            distFieldClassification shouldEqual old
          }
        }
      }
    }
  }

  "part 2" - {
//    "real 1" in {
//      part2(realData, 1) shouldEqual 4
//    }

    "real 130" in {
      part2(realData, 130) shouldEqual 15497
    }

//    "test 1" in {
//      part2(testData, 1) shouldEqual 2
//    }
//
//    "test 6" in {
//      part2(testData, 6) shouldEqual 16
//    }
//
//    "test 7" in {
//      part2(testData, 7) shouldEqual 22
//    }
//
//    "test 8" in {
//      part2(testData, 8) shouldEqual 30
//    }
//
//    "test 10" in {
//      part2(testData, 10) shouldEqual 50
//    }
//
//    "test 50" in {
//      part2(testData, 50) shouldEqual 1594
//    }
//
//    "test 100" in {
//      part2(testData, 100) shouldEqual 6536
//    }
//
//    "test 500" in {
//      part2(testData, 500) shouldEqual 167004
//    }
//
//    "test 1000" in {
//      part2(testData, 1000) shouldEqual 668697
//    }
//
//    "test 5000" in {
//      part2(testData, 5000) shouldEqual 16733044
//    }
//
//    "real" in {
//      part2(realData, 26501365) shouldEqual 0
//    }
  }
}
