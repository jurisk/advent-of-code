package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import Advent14._

class Advent14Spec extends AnyFreeSpec {
  private val test = parseFile("2023/14-test.txt")

  "slideNorth" in {
    val slided   = slideNorth(test)
    val expected = parse("""OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....""")

    debugPrint(test)
    debugPrint(expected)
    debugPrint(slided)

    slided shouldEqual expected
  }

  "1 cycle" in {
    val slided   = cycle(test)
    val expected = parse(""".....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....""")

    slided shouldEqual expected
  }

  "3 cycles" in {
    val test     = parseFile("2023/14-test.txt")
    val slided   = cycles(test, 3)
    val expected = parse(""".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O""")

    slided shouldEqual expected
  }

  "part 1" - {
    "test" in {
      part1(test) shouldEqual 136
    }

    "real" in {
      part1(parseFile("2023/14.txt")) shouldEqual 110821
    }
  }

  "part 2" - {
    "test" in {
      part2(test) shouldEqual 64
    }

    "real" in {
      part2(parseFile("2023/14.txt")) shouldEqual 83516
    }
  }
}
