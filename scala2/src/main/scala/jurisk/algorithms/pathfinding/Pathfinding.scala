package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList
import cats.implicits._
import jurisk.algorithms.pathfinding.Dijkstra.dijkstra
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
    dijkstra[N, Int](
      start,
      n => successors(n).map(x => x -> 1),
      isGoal,
    ).map { case (path, cost) =>
      require(
        path.length - 1 == cost,
        "Expected path to match cost but it didn't",
      )
      path
    }
}
