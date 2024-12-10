package jurisk.adventofcode.y2024

import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._

object Advent10 {
  type Input = Field2D[Int]
  type N     = Long

  private val Start  = 0
  private val Finish = 9

  def parse(input: String): Input =
    Field2D.parseDigitField(input)

  private def countReachableEnds(data: Input, from: Coords2D): Int = {
    def reachableEnds(data: Input, c: Coords2D, current: Int): Set[Coords2D] =
      if (current == Finish) {
        Set(c)
      } else {
        val next = current + 1
        data
          .neighboursFor(c, includeDiagonal = false)
          .filter(n => data.at(n).contains(next))
          .flatMap { n =>
            reachableEnds(data, n, next)
          }
          .toSet
      }

    reachableEnds(data, from, 0).size
  }

  private def countDistinctPaths(
    data: Input,
    c: Coords2D,
    current: Int = Start,
  ): Int =
    if (current == Finish) {
      1
    } else {
      val next = current + 1
      data
        .neighboursFor(c, includeDiagonal = false)
        .filter(n => data.at(n).contains(next))
        .map { n =>
          countDistinctPaths(data, n, next)
        }
        .sum
    }

  private def solve(data: Input, f: (Input, Coords2D) => Int): Int = {
    val candidateTrailHeads = data.filterCoordsByValue(Start)
    val scores              = candidateTrailHeads.map(c => f(data, c))
    scores.sum
  }

  def part1(data: Input): N =
    solve(data, countReachableEnds)

  def part2(data: Input): N =
    solve(data, (data, c) => countDistinctPaths(data, c))

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/10$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
