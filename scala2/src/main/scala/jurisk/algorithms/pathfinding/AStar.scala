package jurisk.algorithms.pathfinding

import cats.implicits._
import cats.data.NonEmptyList
import jurisk.utils.Bounded
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._
import scala.collection.mutable

object AStar {
  /** @param start
    *   The starting node.
    * @param successors
    *   List of successors for a given node, along with the cost of moving from
    *   the given node to this successor.
    * @param heuristic
    *   Approximation of the cost from the given node to the goal. Must not be
    *   greater than the real cost, or a wrong path may be returned!
    * @param success
    *   Is the node reached? A function, not a node, because some problems
    *   require a dynamic solution.
    * @tparam N
    *   Node type
    * @tparam C
    *   Cost type
    * @return
    *   The shortest path starting from `start` up to a node for which `success`
    *   returns `true`, along with the total cost, in a `Some`. If no path can
    *   be found, `None` is returned instead.
    */
  def aStar[N, C: Numeric: Ordering: Bounded](
    start: N,
    neighbours: N => List[(N, C)],
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
    val openSet: mutable.PriorityQueue[N] = mutable.PriorityQueue.apply(start)(
      Ordering.by(fScore).reverse
    )

    while (openSet.nonEmpty) {
      // This operation can occur in O(1) time if openSet is a min-heap or a priority queue
      val current = openSet.dequeue()
      if (isGoal(current))
        return (reconstructPath(cameFrom, current), gScore(current)).some

      neighbours(current) foreach { case (neighbour, d) =>
        // d(current, neighbor) is the weight of the edge from current to neighbor

        // tentative_gScore is the distance from start to the neighbor through current
        val tentativeGScore = gScore(current) + d
        if (tentativeGScore < gScore(neighbour)) {
          // This path to neighbor is better than any previous one. Record it!
          cameFrom.update(neighbour, current)
          gScore.update(neighbour, tentativeGScore)
          fScore.update(neighbour, tentativeGScore + heuristic(neighbour))

          if (openSet.count(_ == neighbour) == 0) {
            openSet.enqueue(neighbour)
          }
        }
      }
    }

    // Open set is empty but goal was never reached
    none
  }
}
