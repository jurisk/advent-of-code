package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent13._
import jurisk.geometry.Field2D
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Advent13Spec extends AnyFreeSpec {
  "part 1" - {
    "test" in {
      part1(parseFile("2023/13-test.txt")) shouldEqual 405
    }

    "real" in {
      part1(parseFile("2023/13.txt")) shouldEqual 36448
    }
  }

  "part 2" - {
    "interesting" in {
      val test = """.#..###.#
                   |.###.##.#
                   |....###.#
                   |.######..
                   |...#.#.#.
                   |...#.#.#.
                   |.######..
                   |....###.#
                   |.###.##.#
                   |.#..###.#
                   |####...##
                   |..##.#..#
                   |..##....#
                   |####...##
                   |.#..###.#""".stripMargin

      val parsed = Field2D.parseBooleanField(test)

      value(parsed) shouldEqual 500
      fixedValue(parsed) shouldEqual 1200
    }

    "test" in {
      val input      = parseFile("2023/13-test.txt")
      val List(a, b) = input
      fixedValue(a) shouldEqual 300
      fixedValue(b) shouldEqual 100
      part2(input) shouldEqual 400
    }

    "real" in {
      part2(parseFile("2023/13.txt")) shouldEqual 35799
    }
  }
}
