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

  private def solve(
    data: Input,
    from: Node,
    to: Node,
    mustVisit: Set[Node],
  ): N = {
    var count = 0L
    Dfs.dfsAllPaths[Node](
      start = from,
      successors = node => data.getOrElse(node, Nil),
      isGoal = _ == to,
      visit = path =>
        if (mustVisit.forall(path.contains)) {
          count += 1
        },
    )
    count
  }

  def part1(data: Input): N =
    solve(data, Start, End, Set.empty)

  def part2(data: Input): N =
    solve(data, "svr", End, Set("dac", "fft"))

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
