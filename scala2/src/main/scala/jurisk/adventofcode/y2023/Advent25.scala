package jurisk.adventofcode.y2023

import jurisk.algorithms.graph.KargersMinCuts
import jurisk.algorithms.pathfinding.ConnectedComponents
import jurisk.collections.immutable.SetOfTwo
import jurisk.collections.immutable.graph.Graph
import jurisk.collections.immutable.graph.Graph.VertexId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

object Advent25 {
  type Input = Seq[SetOfTwo[String]]

  // TODO: Use Graph.toDotUndirectedGraph instead
  private def printEdges(inputs: Seq[SetOfTwo[String]]): Unit = {
    // dot -Tsvg -Kneato 25.dot -o 25.svg

    val edges = inputs
      .map { e =>
        val (a, b) = e.tupleInArbitraryOrder
        s"  $a -- $b"
      }
      .mkString("\n")

    println(s"""
               |graph G {
               |$edges
               |}
               |""".stripMargin)
  }

  def parse(input: String): Input = {
    val inputs: Seq[SetOfTwo[String]] = input.splitLines.flatMap {
      case s"$from: $tos" => tos.split(" ").map(to => SetOfTwo(from, to))
      case s              => s.failedToParse
    }

    printEdges(inputs)
    inputs
  }

  def solve(input: Input): Int = {
    val graph =
      Graph.undirected(input.toSet.map((x: SetOfTwo[String]) => x -> 1L))

    val cuts = KargersMinCuts.minCuts(graph, 3)

    println(s"Cuts: $cuts")

    val cutsAsVertices = cuts.map(_.mapUnsafe(graph.labelToVertex))

    val results = ConnectedComponents.connectedComponents[VertexId](
      graph.allVertices.toList,
      from =>
        graph
          .verticesReachableFrom(from)
          .filterNot { to =>
            cutsAsVertices.contains(SetOfTwo(from, to))
          },
    )

    results.size shouldEqual 2
    results.map(_.size).product
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/25$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${solve(realData)}")
  }
}
