package jurisk.adventofcode.y2025

import jurisk.algorithms.pathfinding.Dfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent11 {
  type Input = Map[String, List[String]]
  type N     = Long

  private val Start = "you"
  private val End   = "out"

  def parse(input: String): Input =
    input.parseLines { line =>
      val (from, targets) =
        line.parsePairUnsafe(": ", identity, _.parseList(' ', identity))
      from -> targets
    }.toMap

  def part1(data: Input): N = {
    var count = 0L
    Dfs.dfsAllPaths[String](
      start = Start,
      successors = node => data.getOrElse(node, Nil),
      isGoal = _ == End,
      visit = _ => count += 1,
    )
    count
  }

  def part2(data: Input): N =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/11$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
