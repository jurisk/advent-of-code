package jurisk.adventofcode.y2023

import jurisk.geometry.Coordinates2D
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

import Advent11._

class Advent11Spec extends AnyFreeSpec {
  "expand" - {
    "test 1" in {
      expand(
        ArraySeq(
          Coordinates2D[BigInt](0, 0),
          Coordinates2D[BigInt](0, 1),
        ),
        2,
      ) shouldEqual ArraySeq(
        Coordinates2D[BigInt](0, 0),
        Coordinates2D[BigInt](0, 1),
      )
    }

    "test 2" in {
      expand(
        ArraySeq(
          Coordinates2D[BigInt](0, 0),
          Coordinates2D[BigInt](0, 2),
        ),
        2,
      ) shouldEqual ArraySeq(
        Coordinates2D[BigInt](0, 0),
        Coordinates2D[BigInt](0, 3),
      )
    }

    "test 3" in {
      expand(
        ArraySeq(
          Coordinates2D[BigInt](0, 0),
          Coordinates2D[BigInt](0, 3),
        ),
        2,
      ) shouldEqual ArraySeq(
        Coordinates2D[BigInt](0, 0),
        Coordinates2D[BigInt](0, 5),
      )
    }

    "test 4" in {
      expand(
        ArraySeq(
          Coordinates2D[BigInt](0, 0),
          Coordinates2D[BigInt](0, 1),
        ),
        100_000,
      ) shouldEqual ArraySeq(
        Coordinates2D[BigInt](0, 0),
        Coordinates2D[BigInt](0, 1),
      )
    }

    "test 5" in {
      expand(
        ArraySeq(
          Coordinates2D[BigInt](0, 0),
          Coordinates2D[BigInt](0, 2),
        ),
        100_000,
      ) shouldEqual ArraySeq(
        Coordinates2D[BigInt](0, 0),
        Coordinates2D[BigInt](0, 100_001),
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
