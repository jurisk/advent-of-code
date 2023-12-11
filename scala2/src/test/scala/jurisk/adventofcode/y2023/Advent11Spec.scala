package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent11._
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

class Advent11Spec extends AnyFreeSpec {
  "expandRows" - {
    "test 1" in {
      expandRows(
        ArraySeq(
          BigCoords2D(0, 0),
          BigCoords2D(0, 1),
        ),
        2,
      ) shouldEqual ArraySeq(
        BigCoords2D(0, 0),
        BigCoords2D(0, 1),
      )
    }

    "test 2" in {
      expandRows(
        ArraySeq(
          BigCoords2D(0, 0),
          BigCoords2D(0, 2),
        ),
        2,
      ) shouldEqual ArraySeq(
        BigCoords2D(0, 0),
        BigCoords2D(0, 3),
      )
    }

    "test 3" in {
      expandRows(
        ArraySeq(
          BigCoords2D(0, 0),
          BigCoords2D(0, 3),
        ),
        2,
      ) shouldEqual ArraySeq(
        BigCoords2D(0, 0),
        BigCoords2D(0, 5),
      )
    }

    "test 4" in {
      expandRows(
        ArraySeq(
          BigCoords2D(0, 0),
          BigCoords2D(0, 1),
        ),
        100_000,
      ) shouldEqual ArraySeq(
        BigCoords2D(0, 0),
        BigCoords2D(0, 1),
      )
    }

    "test 5" in {
      expandRows(
        ArraySeq(
          BigCoords2D(0, 0),
          BigCoords2D(0, 2),
        ),
        100_000,
      ) shouldEqual ArraySeq(
        BigCoords2D(0, 0),
        BigCoords2D(0, 100_001),
      )
    }

  }

  "part 1" - {
    "test" in {
      part1(parseFile("2023/11-test.txt")) shouldEqual 374
    }

    "real" in {
      part1(parseFile("2023/11.txt")) shouldEqual 9686930
    }
  }

  "part 2" - {
    "test 1" in {
      solve(parseFile("2023/11-test.txt"), 10) shouldEqual 1030
    }

    "test 2" in {
      solve(parseFile("2023/11-test.txt"), 100) shouldEqual 8410
    }

    "real" in {
      part2(parseFile("2023/11.txt")) shouldEqual BigInt("630728425490")
    }
  }
}
