package jurisk.algorithms.graph

import jurisk.collections.immutable.SetOfTwo
import jurisk.collections.immutable.graph.Graph
import jurisk.collections.immutable.graph.Graph.VertexId
import jurisk.collections.mutable.DisjointSets
import jurisk.utils.CollectionOps.VectorOps

import scala.annotation.tailrec
import scala.util.Random

// This is Karger's and not the more efficient Karger-Stein, and doesn't do `T` times, but expects that we know
// how many cuts we should get.
// https://en.wikipedia.org/wiki/Karger%27s_algorithm
object KargersMinCuts {
  private def attemptCuts[T](graph: Graph[T]): Seq[SetOfTwo[T]] = {
    val disjointSets = DisjointSets[VertexId](graph.allVertices: _*)
    val allEdges     = graph.allEdges.toVector.map { case (from, _, to) =>
      SetOfTwo(from, to)
    }

    var edges = allEdges

    var vertexCount = graph.vertexCount
    while (vertexCount > 2) {
      val e = {
        // choose e ∈ E uniformly at random
        val selectedEdgeIndex = Random.nextInt(edges.size)
        val result            = edges(selectedEdgeIndex)

        // TODO: Does this removal improve or worsen the running time?
        edges = edges.removeAt(selectedEdgeIndex)
        result
      }

      e.map(disjointSets.find).toList match {
        case a :: b :: Nil if a != b =>
          disjointSets.union(a, b)
          vertexCount -= 1
        case _                       =>
      }
    }

    // TODO: allEdges or edges ?
    allEdges
      .filter { e =>
        e.map(disjointSets.find).toList match {
          case a :: b :: Nil if a != b =>
            true
          case _                       =>
            false
        }
      }
      .map { e =>
        e.mapUnsafe(graph.labelFor)
      }
  }

  @tailrec
  def minCuts[T](graph: Graph[T], numCuts: Int): Set[SetOfTwo[T]] = {
    val candidate = attemptCuts(graph)
    if (candidate.size <= numCuts) {
      candidate.toSet
    } else {
      minCuts(graph, numCuts)
    }
  }
}
