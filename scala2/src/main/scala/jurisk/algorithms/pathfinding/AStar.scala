package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList
import cats.implicits._
import jurisk.utils.Bounded

import scala.collection.mutable
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

object AStar {

  /** @param start
    *   The starting node.
    * @param neighbours
    *   List of successors for a given node, along with the cost of moving from
    *   the given node to this successor.
    * @param heuristic
    *   Approximation of the cost from the given node to the goal. Must not be
    *   greater than the real cost, or a wrong path may be returned!
    * @param isGoal
    *   Is the node reached? A function, not a node, because some problems
    *   require a dynamic solution.
    * @tparam N
    *   Node type.
    * @tparam C
    *   Cost type.
    * @return
    *   The shortest path starting from `start` up to a node for which `success`
    *   returns `true`, along with the total cost, in a `Some`. If no path can
    *   be found, `None` is returned instead.
    */
  def aStar[N, C: Numeric: Bounded](
    start: N,
    neighbours: N => IterableOnce[(N, C)],
    heuristic: N => C,
    isGoal: N => Boolean,
  ): Option[(NonEmptyList[N], C)] = {
    val Zero     = implicitly[Numeric[C]].zero
    val MaxValue = implicitly[Bounded[C]].maxValue

    def reconstructPath(
      cameFrom: mutable.Map[N, N],
      _current: N,
    ): NonEmptyList[N] = {
      var current   = _current
      var totalPath = NonEmptyList.one(current)

      while (cameFrom.contains(current)) {
        current = cameFrom(current)
        totalPath = totalPath.prepend(current)
      }

      totalPath
    }

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    val cameFrom: mutable.Map[N, N] = mutable.Map.empty

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    val gScore: mutable.Map[N, C] =
      mutable.Map(start -> Zero).withDefaultValue(MaxValue)

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n.
    val fScore: mutable.Map[N, C] =
      mutable.Map(start -> heuristic(start)).withDefaultValue(MaxValue)

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    val openSet: mutable.PriorityQueue[(N, C)] =
      mutable.PriorityQueue.apply((start, fScore(start)))(
        Ordering[C].reverse.contramap { case (_, c) => c }
      )
    while (openSet.nonEmpty) {
      // This operation can occur in O(1) time if openSet is a min-heap or a priority queue
      val (current, dequeuedCost) = openSet.dequeue()

      // We may have inserted several nodes into the priority queue if we found a better way.
      // We should ensure we are dealing with the best path and ignore the others.
      if (dequeuedCost <= fScore(current)) {
        if (isGoal(current))
          return (reconstructPath(cameFrom, current), gScore(current)).some

        neighbours(current).iterator foreach { case (neighbour, d) =>
          // d(current, neighbor) is the weight of the edge from current to neighbor

          // tentative_gScore is the distance from start to the neighbor through current
          val tentativeGScore = gScore(current) + d
          if (tentativeGScore < gScore(neighbour)) {
            // This path to neighbor is better than any previous one. Record it!
            cameFrom.update(neighbour, current)
            gScore.update(neighbour, tentativeGScore)
            fScore.update(neighbour, tentativeGScore + heuristic(neighbour))

            // We are forced to insert potentially duplicate elements as there is no "reprioritise" on PriorityQueue
            openSet.enqueue((neighbour, fScore(neighbour)))
          }
        }
      }

    }

    // Open set is empty but goal was never reached
    none
  }
}
