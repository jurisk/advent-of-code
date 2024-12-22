package jurisk.adventofcode.y2024

import Advent22._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent22Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "next" in {
      val a = next(123)
      a shouldEqual 15887950
    }

    "test 1" in {
      nthSecretNumber(1, 2000) shouldEqual 8685429
    }

    "test" in {
      part1(testData0) shouldEqual 37327623
    }

    "real" in {
      part1(realData) shouldEqual 13022553808L
    }
  }

  "part 2" - {
    "bananasAndDiffs" in {
      val (bananas123, diffs123) = bananasAndDiffs(123)
      bananas123.length shouldEqual 2001
      diffs123.length shouldEqual 2000
      bananas123.take(10) shouldEqual Array(3, 0, 6, 5, 4, 4, 6, 4, 4, 2)
      bananas123.contains(Long.MaxValue) shouldEqual false
      diffs123.take(9) shouldEqual Array(-3, 6, -1, -1, 0, 2, -2, 0, -2)
      diffs123.contains(Long.MaxValue) shouldEqual false
    }

    "bananasFromSequence" in {
      bananasFromSequence(
        bananasAndDiffs(123),
        IndexedSeq(-1, -2, 0, 2),
      ) shouldEqual Some(6)

      val seq: IndexedSeq[N] = IndexedSeq(-2, 1, -1, 3)
      bananasFromSequence(bananasAndDiffs(1), seq) shouldEqual Some(7)
      bananasFromSequence(bananasAndDiffs(2), seq) shouldEqual Some(7)
      bananasFromSequence(bananasAndDiffs(3), seq) shouldEqual None
      bananasFromSequence(bananasAndDiffs(2024), seq) shouldEqual Some(9)
    }

    "theMap" in {
      val seq: List[N] = List(-2, 1, -1, 3)
      createBananaMap(1).get(seq) shouldEqual Some(7)
      createBananaMap(2).get(seq) shouldEqual Some(7)
      createBananaMap(3).get(seq) shouldEqual None
      createBananaMap(2024).get(seq) shouldEqual Some(9)
    }

    "test" in {
      part2(testData1) shouldEqual 7 + 7 + 9
    }

    "real" in {
      part2(realData) shouldEqual 1555
    }
  }
}
