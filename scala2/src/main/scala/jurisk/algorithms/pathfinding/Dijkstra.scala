package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList
import jurisk.algorithms.pathfinding.AStar.aStar
import jurisk.utils.Bounded
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._
import scala.collection.mutable

object Dijkstra {

  /** @param start
    *   The starting node.
    * @param successors
    *   List of successors for a given node, along with the cost of moving from
    *   the given node to this successor.
    * @tparam N
    *   Node type.
    * @tparam C
    *   Cost type.
    * @return
    *   A map where every reachable node (not including `start`) is associated
    *   with the optimal parent node and a cost from the start node.
    */
  // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
  //  1  function Dijkstra(Graph, source):
  def dijkstraAll[N, C: Numeric: Ordering: Bounded](
    start: N,
    successors: N => List[(N, C)],
  ): Map[N, (N, C)] = {
    val Zero     = implicitly[Numeric[C]].zero
    val MaxValue = implicitly[Bounded[C]].maxValue

    //  3      for each vertex v in Graph.Vertices:
    //  4          dist[v] ← INFINITY
    val dist: mutable.Map[N, C]         = mutable.Map().withDefaultValue(MaxValue)
    //  5          prev[v] ← UNDEFINED
    val prev: mutable.Map[N, N]         = mutable.Map()
    //  6          add v to Q
    val queue: mutable.PriorityQueue[N] = mutable.PriorityQueue.empty(
      Ordering.by(dist).reverse
    )
    queue.enqueue(start)
    //  7      dist[source] ← 0
    dist.update(start, Zero)

    //  9      while Q is not empty:
    while (queue.nonEmpty) {
      // 10          u ← vertex in Q with min dist[u]

      // 11          remove u from Q
      val u = queue.dequeue()

      // 13          for each neighbor v of u still in Q:
      successors(u) foreach { case (v, distance) =>
        // 14              alt ← dist[u] + Graph.Edges(u, v)
        val alt = dist(u) + distance

        // 15              if alt < dist[v]:
        if (alt < dist(v)) {
          // 16                  dist[v] ← alt
          dist.update(v, alt)
          // 17                  prev[v] ← u
          prev.update(v, u)

          if (queue.count(_ == v) == 0) {
            queue.enqueue(v)
          }
        }
      }
    }

    dist.remove(start) // Not returning start
    dist.map { case (n, c) =>
      val parent = prev(n)
      (n, (parent, c))
    }.toMap
  }

  def dijkstra[N, C: Numeric: Ordering: Bounded](
    start: N,
    successors: N => List[(N, C)],
    success: N => Boolean,
  ): Option[(NonEmptyList[N], C)] =
    dijkstraUsingAStar(start, successors, success)

  private def dijkstraUsingAStar[N, C: Numeric: Ordering: Bounded](
    start: N,
    successors: N => List[(N, C)],
    success: N => Boolean,
  ): Option[(NonEmptyList[N], C)] = {
    val Zero = implicitly[Numeric[C]].zero
    aStar[N, C](start, successors, _ => Zero, success)
  }
}
