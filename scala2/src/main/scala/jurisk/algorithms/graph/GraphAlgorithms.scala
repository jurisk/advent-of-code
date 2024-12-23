package jurisk.algorithms.graph

import scala.collection.mutable

object GraphAlgorithms {
  def createAdjacencyMap[T](undirectedEdges: Seq[(T, T)]): Map[T, Set[T]] = {
    val adjacencyMap = mutable.Map.empty[T, Set[T]]

    undirectedEdges.foreach { case (a, b) =>
      adjacencyMap(a) = adjacencyMap.getOrElse(a, Set.empty) + b
      adjacencyMap(b) = adjacencyMap.getOrElse(b, Set.empty) + a
    }

    adjacencyMap.toMap
  }

  // https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
  def enumerateMaximumCliques[T](
    neighbours: Map[T, Set[T]],
    found: Set[T] => Unit,
  ): Unit = {
    def bronKerbosch(
      r: Set[T],
      p: mutable.Set[T],
      x: mutable.Set[T],
    ): Unit =
      // if P and X are both empty then
      if (p.isEmpty && x.isEmpty) {
        // report R as a maximal clique
        found(r)
      } else {
        // choose a pivot vertex u in P ⋃ X
        val pivot = p.union(x).maxBy(neighbours(_).size)

        // for each vertex v in P \ N(u) do
        p.diff(neighbours(pivot)).foreach { v =>
          // BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          val nV = neighbours(v)
          bronKerbosch(
            r + v,
            p.intersect(nV),
            x.intersect(nV),
          )
          // P := P \ {v}
          p -= v
          // X := X ⋃ {v}
          x += v
        }
      }

    val initialP = mutable.Set(neighbours.keys.toSeq: _*)
    bronKerbosch(Set.empty, initialP, mutable.Set.empty)
  }
}
