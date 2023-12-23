package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent21._
import org.scalatest.matchers.should.Matchers._

class Advent21Spec extends AnyFreeSpec {
  private def testData          = parseFile(fileName("-test"))
  private def realData          = parseFile(fileName(""))
  private def test131Empty      = parseFile(fileName("-test-131-empty"))
  private def test131SingleRock = parseFile(fileName("-test-131-single-rock"))

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

  "FieldCounts" - {
    val emptyCounts = InnerCounts(Map.empty, 0, 0)

    "131" - {
      val size = 131

      "259" in {
        FieldCounts.make(259, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(
            Map(63L -> 1L, 63L + size -> 1L),
            0,
            0,
          ),
          corner = InnerCounts(
            Map(128L -> 1L),
            0,
            0,
          ),
        )
      }

      "261" in {
        FieldCounts.make(261, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(
            Map(65L -> 1L),
            0,
            1L,
          ),
          corner = InnerCounts(
            Map(130L -> 1L),
            0,
            0,
          ),
        )
      }
    }

    "3" - {
      val size = 3

      "0" in {
        FieldCounts.make(0, size) shouldEqual FieldCounts(
          edgeCenter = emptyCounts,
          corner = emptyCounts,
        )
      }

      "1" in {
        FieldCounts.make(1, size) shouldEqual FieldCounts(
          edgeCenter = emptyCounts,
          corner = emptyCounts,
        )
      }

      "2" in {
        FieldCounts.make(2, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(1L -> 1L), 0, 0),
          corner = emptyCounts,
        )
      }

      "3" in {
        FieldCounts.make(3, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 0, 0),
          corner = emptyCounts,
        )
      }

      "4" in {
        FieldCounts.make(4, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(3L -> 1L), 0, 0),
          corner = InnerCounts(Map(1L -> 1L), 0, 0),
        )
      }

      "5" in {
        FieldCounts.make(5, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(1L -> 1L), 0, 1),
          corner = InnerCounts(Map(2L -> 1L), 0, 0),
        )
      }

      "6" in {
        FieldCounts.make(6, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 0, 1),
          corner = InnerCounts(Map(3L -> 1L), 0, 0),
        )
      }

      "7" in {
        FieldCounts.make(7, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(3L -> 1L), 0, 1),
          corner = InnerCounts(Map(1L -> 2L, 4L -> 1L), 0, 0),
        )
      }
    }

    "5" - {
      val size = 5

      "3" in {
        FieldCounts.make(3, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(1L -> 1L), 0, 0),
          corner = emptyCounts,
        )
      }

      "4" in {
        FieldCounts.make(4, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 0, 0),
          corner = emptyCounts,
        )
      }

      "5" in {
        FieldCounts.make(5, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(3L -> 1L), 0, 0),
          corner = emptyCounts,
        )
      }

      "8" in {
        FieldCounts.make(8, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(6L -> 1L, 1L -> 1L), 0, 0),
          corner = InnerCounts(Map(3L -> 1L), 0, 0),
        )
      }

      "14" in {
        FieldCounts.make(14, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(2L -> 1L), 1L, 1L),
          corner = InnerCounts(Map(4L -> 2L), 1L, 0L),
        )
      }

      "26" in {
        FieldCounts.make(26, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(4L -> 1L), 2L, 2L),
          corner = InnerCounts(Map(6L -> 4L, 1L -> 5L), 4L, 2L),
        )
      }

      "28" in {
        FieldCounts.make(28, size) shouldEqual FieldCounts(
          edgeCenter = InnerCounts(Map(6L -> 1L, 1L -> 1L), 2L, 2L),
          corner = InnerCounts(Map(3L -> 5L, 8L -> 4L), 4L, 2L),
        )
      }
    }
  }

  "comparison" - {
    List(
      "empty",
      "single-rock",
      "simple-wall",
      "unpassable",
      "9-another-wall",
    ) map { name =>
      val test = parseFile(fileName(s"-test-$name"))

      name - {
        val MaxN = 100
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

    "real 131" in {
      part2(realData, 131) shouldEqual 15724
    }

    "real 133" in {
      part2(realData, 133) shouldEqual 16194
    }

    "real 257" in {
      part2(realData, 257) shouldEqual 60136
    }

    "real 259" in {
      part2(realData, 259) shouldEqual 61056
    }

    "real 260" in {
      part2(realData, 260) shouldEqual 61553
    }

    "real 261" in {
      part2(realData, 261) shouldEqual 62004
    }

    "real 262" in {
      part2(realData, 262) shouldEqual 62457
    }

    "real 263" in {
      part2(realData, 263) shouldEqual 62930
    }

    "real 264" in {
      part2(realData, 264) shouldEqual 63390
    }

    "real 265" in {
      part2(realData, 265) shouldEqual 63904
    }

    "real 350" in {
      part2(realData, 350) shouldEqual 111444
    }

    "real 351" in {
      part2(realData, 351) shouldEqual 112092
    }

    "real 352" in {
      part2(realData, 352) shouldEqual 112697
    }

    "real 353" in {
      part2(realData, 353) shouldEqual 113381
    }

    "131-empty 263" in {
      part2(test131Empty, 263) shouldEqual 69696
    }

    "131-single-rock 261" in {
      part2(test131SingleRock, 261) shouldEqual 68640
    }

    "131-single-rock 400" in {
      part2(test131SingleRock, 400) shouldEqual 160789
    }

    "131-single-rock 401" in {
      part2(test131SingleRock, 401) shouldEqual 161592
    }

    "131-single-rock 263" in {
      part2(test131SingleRock, 263) shouldEqual 69692
    }

    "real 400" in {
      part2(realData, 400) shouldEqual 145254
    }

    "test 1" in {
      part2Old(testData, 1) shouldEqual 2
    }

    "test 6" in {
      part2Old(testData, 6) shouldEqual 16
    }

    "test 7" in {
      part2Old(testData, 7) shouldEqual 22
    }

    "test 8" in {
      part2Old(testData, 8) shouldEqual 30
    }

    "test 10" in {
      part2Old(testData, 10) shouldEqual 50
    }

    "test 50" in {
      part2Old(testData, 50) shouldEqual 1594
    }

    "test 100" in {
      part2Old(testData, 100) shouldEqual 6536
    }

    "test 500" ignore {
      part2Old(testData, 500) shouldEqual 167004
    }

    "test 1000" ignore {
      part2Old(testData, 1000) shouldEqual 668697
    }

    "test 5000" ignore {
      part2Old(testData, 5000) shouldEqual 16733044
    }

    "real" in {
      part2FieldClassification(realData, 26501365) shouldEqual 634549784009844L
    }
  }
}
