package jurisk.adventofcode.y2024

import Advent22._
import cats.implicits.{catsSyntaxOptionId, none}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent22Spec extends AnyFreeSpec {
  private def testData0 = parseFile(fileName("-test-00"))
  private def testData1 = parseFile(fileName("-test-01"))
  private def realData  = parseFile(fileName(""))

  "part 1" - {
    "next" in {
      next(123) shouldEqual 15887950
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
      bananas123.take(10) shouldEqual IndexedSeq(3, 0, 6, 5, 4, 4, 6, 4, 4, 2)
      bananas123.contains(Long.MaxValue) shouldEqual false
      diffs123.take(9) shouldEqual IndexedSeq(-3, 6, -1, -1, 0, 2, -2, 0, -2)
      diffs123.contains(Long.MaxValue) shouldEqual false
    }

    "bananasFromSequence" in {
      bananasFromSequence(
        bananasAndDiffs(123),
        IndexedSeq(-1, -2, 0, 2),
      ) shouldEqual Some(6)

      val seq: IndexedSeq[N] = IndexedSeq(-2, 1, -1, 3)
      bananasFromSequence(bananasAndDiffs(1), seq) shouldEqual 7.some
      bananasFromSequence(bananasAndDiffs(2), seq) shouldEqual 7.some
      bananasFromSequence(bananasAndDiffs(3), seq) shouldEqual none
      bananasFromSequence(bananasAndDiffs(2024), seq) shouldEqual 9.some
    }

    "theMap" in {
      val seq: IndexedSeq[N] = IndexedSeq(-2, 1, -1, 3)
      createBananaMap(1).get(seq) shouldEqual 7.some
      createBananaMap(2).get(seq) shouldEqual 7.some
      createBananaMap(3).get(seq) shouldEqual none
      createBananaMap(2024).get(seq) shouldEqual 9.some
    }

    "test" in {
      part2(testData1) shouldEqual 7 + 7 + 9
    }

    "real" ignore {
      part2(realData) shouldEqual 1555
    }
  }
}
