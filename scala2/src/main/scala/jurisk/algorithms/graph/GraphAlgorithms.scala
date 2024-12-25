package jurisk.algorithms.graph

import jurisk.collections.immutable.SetOfTwo
import mouse.all.booleanSyntaxMouse

import scala.collection.mutable

object GraphAlgorithms {
  def createAdjacencyMapDirected[T](
    directedEdges: Seq[(T, T)]
  ): Map[T, Set[T]] = {
    val adjacencyMap = mutable.Map.empty[T, Set[T]]

    directedEdges.foreach { case (a, b) =>
      adjacencyMap(a) = adjacencyMap.getOrElse(a, Set.empty) + b
      adjacencyMap(b) = adjacencyMap.getOrElse(b, Set.empty)
    }

    adjacencyMap.toMap
  }

  def createAdjacencyMapUndirected[T](
    undirectedEdges: Seq[SetOfTwo[T]]
  ): Map[T, Set[T]] = {
    val directedEdges = undirectedEdges.flatMap { s =>
      val (a, b) = s.tupleInArbitraryOrder
      Set(
        (a, b),
        (b, a),
      )
    }
    createAdjacencyMapDirected(directedEdges)
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

  // https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
  def topologicalSort[T](graph: Map[T, Set[T]]): Option[List[T]] = {
    // Compute in-degrees for each node
    val inDegree = mutable.Map[T, Int]().withDefaultValue(0)
    graph.values.flatten.foreach(node => inDegree(node) += 1)
    graph.keys.foreach(node =>
      inDegree(node) += 0
    ) // Ensure all nodes are present in in-degree map

    // Initialize a queue with nodes having in-degree 0
    val queue = mutable.Queue[T]()
    inDegree.foreach { case (node, degree) =>
      if (degree == 0) queue.enqueue(node)
    }

    // Perform Kahn's algorithm
    val sortedList = mutable.ListBuffer[T]()
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      sortedList.append(current)

      for (neighbor <- graph.getOrElse(current, List())) {
        inDegree(neighbor) -= 1
        if (inDegree(neighbor) == 0) queue.enqueue(neighbor)
      }
    }

    // Check for a cycle
    (sortedList.size == graph.size).option(sortedList.toList)
  }
}
