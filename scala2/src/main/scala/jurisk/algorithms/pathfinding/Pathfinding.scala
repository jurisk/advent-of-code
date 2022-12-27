package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList
import jurisk.algorithms.pathfinding.Dijkstra.dijkstraWithIdenticalCosts
import jurisk.utils.Bounded._

object Pathfinding {
  def shortestPathLength[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[Int] = shortestPath(start, successors, isGoal).map(_.length - 1)

  def shortestPath[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[NonEmptyList[N]] =
    dijkstraWithIdenticalCosts[N, Int](
      start,
      successors,
      isGoal,
    ).map { case (path, cost) =>
      require(
        path.length - 1 == cost,
        "Expected path to match cost but it didn't",
      )
      path
    }
}
