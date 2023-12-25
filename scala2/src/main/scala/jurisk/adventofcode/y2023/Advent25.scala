package jurisk.adventofcode.y2023

import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.algorithms.pathfinding.{Bfs, ConnectedComponents}
import jurisk.collections.SetOfTwo
import jurisk.graph.Graph
import jurisk.graph.Graph.VertexId
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent25 {
  type Input = Seq[SetOfTwo[String]]

  private def printEdges(inputs: Seq[SetOfTwo[String]]): Unit = {
    // dot -Tsvg -Kneato 25.dot -o 25.svg

    val edges = inputs
      .map { e =>
        val (a, b) = e.tupleInArbitraryOrder
        s"$a -- $b"
      }
      .mkString("\n")

    println(s"""
               |graph G {
               |  $edges
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

  // TODO: way too slow - consider https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm or smth
  def part1(input: Input): Int = {
    val graph =
      Graph.undirected(input.toSet.map((x: SetOfTwo[String]) => x -> 1L))
    val edges = input.map { s =>
      val (a, b) = s.tupleInArbitraryOrder
      SetOfTwo(graph.labelToVertex(a), graph.labelToVertex(b))
    }

    //    val remove = Set(SetOfTwo("lmg", "krx"), SetOfTwo("tnr", "vzb"), SetOfTwo("tqn", "tvf"))
    //    val remove = Set(SetOfTwo("hfx", "pzl"), SetOfTwo("bvb", "cmg"), SetOfTwo("nvd", "jqt"))

    println(s"Edges: ${edges.size}")
    val results = edges.combinations(3).flatMap { list =>
      val prohibited = list.toSet

      val results = ConnectedComponents.connectedComponents[VertexId](
        graph.allVertices.toList,
        x =>
          graph
            .verticesReachableFrom(x)
            .filterNot { to =>
              prohibited.contains(SetOfTwo(x, to))
            }
            .toSeq,
      )

      if (results.size == 2) {
        results
          .map { r =>
            r.size
          }
          .product
          .some
      } else {
        none
      }
    }

    results.toSeq.distinct.singleResultUnsafe
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/25$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
  }
}
