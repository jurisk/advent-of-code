package jurisk.adventofcode.y2023

import jurisk.geometry.Field2D
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent13._

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

      repairedReflection(parsed) shouldEqual Reflection.Horizontal(12)
      singleReflection(parsed).value shouldEqual 500
      repairedReflection(parsed).value shouldEqual 1200
    }

    "test" in {
      val input      = parseFile("2023/13-test.txt")
      val List(a, b) = input
      repairedReflection(a).value shouldEqual 300
      repairedReflection(b).value shouldEqual 100
      part2(input) shouldEqual 400
    }

    "real" in {
      part2(parseFile("2023/13.txt")) shouldEqual 35799
    }
  }
}
