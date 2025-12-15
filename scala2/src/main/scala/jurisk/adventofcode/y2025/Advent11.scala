package jurisk.adventofcode.y2025

import jurisk.algorithms.pathfinding.Dfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent11 {
  private type Node = String

  type Input = Map[Node, List[Node]]
  type N     = Long

  private val Start: Node = "you"
  private val End: Node   = "out"

  def parse(input: String): Input =
    input.parseLines { line =>
      val (from, targets) =
        line.parsePairUnsafe(": ", identity, _.parseList(' ', identity))
      from -> targets
    }.toMap

  private def countPaths(data: Input, from: Node, to: Node): N = {
    val startTime = System.currentTimeMillis()
    println(s"  countPaths($from -> $to) starting...")
    var count     = 0L
    Dfs.dfsAllPaths[Node](
      start = from,
      successors = node => data.getOrElse(node, Nil),
      isGoal = _ == to,
      visit = _ => {
        count += 1
        if (count % 1_000 == 0) println(s"    ... $count paths found")
      },
    )
    val elapsed   = System.currentTimeMillis() - startTime
    println(s"  countPaths($from -> $to) = $count (${elapsed}ms)")
    count
  }

  def part1(data: Input): N =
    countPaths(data, Start, End)

  def part2(data: Input): N = {
    val (a, b) = ("dac", "fft")
    // Paths visiting both a and b = paths where a comes before b + paths where b comes before a
    countPaths(data, "svr", a) * countPaths(data, a, b) * countPaths(
      data,
      b,
      End,
    ) +
      countPaths(data, "svr", b) * countPaths(data, b, a) * countPaths(
        data,
        a,
        End,
      )
  }

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
