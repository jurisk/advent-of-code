package jurisk.algorithms.graph

import jurisk.collections.immutable.SetOfTwo
import jurisk.collections.immutable.graph.Graph
import jurisk.collections.immutable.graph.Graph.VertexId
import jurisk.collections.mutable.DisjointSets

import scala.annotation.tailrec
import scala.util.Random

// This is Karger's and not the more efficient Karger-Stein, and doesn't do `T` times, but expects that we know
// how many cuts we should get.
// https://en.wikipedia.org/wiki/Karger%27s_algorithm
object KargersMinCuts {
  private def attemptCuts[T](graph: Graph[T]): Set[SetOfTwo[T]] = {
    val disjointSets = DisjointSets[VertexId](graph.allVertices: _*)
    val edges        = graph.allEdges.toVector.map { case (from, _, to) =>
      SetOfTwo(from, to)
    }

    var vertexCount = graph.vertexCount
    while (vertexCount > 2) {
      val e = {
        // choose e ∈ E uniformly at random
        val selectedEdgeIndex = Random.nextInt(edges.size)
        // We tried removing "e" from "edges" here but it didn't improve the running time
        edges(selectedEdgeIndex)
      }

      val (a, b) = e.tupleInArbitraryOrder
      if (disjointSets.differentSets(a, b)) {
        disjointSets.union(a, b)
        vertexCount -= 1
      }
    }

    edges
      .filter { e =>
        val (a, b) = e.tupleInArbitraryOrder
        disjointSets.differentSets(a, b)
      }
      .map { e =>
        e.mapUnsafe(graph.labelFor)
      }
      .toSet
  }

  @tailrec
  def minCuts[T](graph: Graph[T], numCuts: Int): Set[SetOfTwo[T]] = {
    val candidate = attemptCuts(graph)
    if (candidate.size <= numCuts) {
      candidate
    } else {
      minCuts(graph, numCuts)
    }
  }
}
