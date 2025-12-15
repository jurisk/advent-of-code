package jurisk.adventofcode.y2025

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.BitSet

object Advent11 {
  private type Node   = String
  private type NodeId = Int

  type Input = Map[Node, List[Node]]
  type N     = Long

  private val End: Node = "out"

  def parse(input: String): Input =
    input.parseLines { line =>
      val (from, targets) =
        line.parsePairUnsafe(": ", identity, _.parseList(' ', identity))
      from -> targets
    }.toMap

  private def removeNodes(data: Input, nodes: Set[Node]): Input =
    data.view
      .filterKeys(k => !nodes.contains(k))
      .mapValues(vs => vs.filterNot(nodes.contains))
      .toMap

  private def buildNodeMap(data: Input): Map[Node, NodeId] = {
    val allNodes = data.keys ++ data.values.flatten
    allNodes.toSet.zipWithIndex.toMap
  }

  private def countPaths(data: Input, from: Node, to: Node): N = {
    val startTime = System.currentTimeMillis()
    println(s"  countPaths($from -> $to) starting...")

    val nodeMap                              = buildNodeMap(data)
    val adjacency: Map[NodeId, List[NodeId]] =
      data.map { case (k, vs) => nodeMap(k) -> vs.map(nodeMap) }

    val startId = nodeMap(from)
    val endId   = nodeMap(to)

    var count = 0L

    def backtrack(current: NodeId, visited: BitSet): Unit =
      if (!visited.contains(current)) {
        if (current == endId) {
          count += 1
          if (count % 1000 == 0) println(s"    ... $count paths found")
        } else {
          val newVisited = visited + current
          adjacency.getOrElse(current, Nil).foreach { next =>
            backtrack(next, newVisited)
          }
        }
      }

    backtrack(startId, BitSet.empty)

    val elapsed = System.currentTimeMillis() - startTime
    println(s"  countPaths($from -> $to) = $count (${elapsed}ms)")
    count
  }

  def part1(data: Input): N = {
    val Start: Node = "you"
    countPaths(data, Start, End)
  }

  def part2(data: Input): N = {
    val Start: Node = "svr"
    val (a, b)      = ("dac", "fft")

    // Paths visiting both a and b = paths where a comes before b + paths where b comes before a
    // Each segment must not pass through the other waypoints
    val startToA = countPaths(removeNodes(data, Set(b, End)), Start, a)
    val aToB     = countPaths(removeNodes(data, Set(Start, End)), a, b)
    val bToEnd   = countPaths(removeNodes(data, Set(Start, a)), b, End)

    val startToB = countPaths(removeNodes(data, Set(a, End)), Start, b)
    val bToA     = countPaths(removeNodes(data, Set(Start, End)), b, a)
    val aToEnd   = countPaths(removeNodes(data, Set(Start, b)), a, End)

    startToA * aToB * bToEnd + startToB * bToA * aToEnd
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
