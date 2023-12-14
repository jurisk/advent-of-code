package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent14._
import jurisk.geometry.Field2D
import org.scalatest.matchers.should.Matchers._

class Advent14Spec extends AnyFreeSpec {
  "qq" in {
    val test     = parseFile("2023/14-test.txt")
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

    debugPrint(slided)

    slided shouldEqual expected
  }

  "1 cycle" in {
    val test     = parseFile("2023/14-test.txt")
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

    debugPrint(slided)

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

    debugPrint(slided)

    slided shouldEqual expected
  }

  "part 1" - {
    "test" in {
      part1(parseFile("2023/14-test.txt")) shouldEqual 136
    }

    "real" in {
      part1(parseFile("2023/14.txt")) shouldEqual 110821
    }
  }

  "part 2" - {
    "test" in {
      part2(parseFile("2023/14-test.txt")) shouldEqual 64
    }

    "real" in {
      part2(parseFile("2023/14.txt")) shouldEqual 83516
    }
  }
}
