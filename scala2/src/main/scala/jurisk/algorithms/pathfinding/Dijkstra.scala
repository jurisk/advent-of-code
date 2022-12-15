package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList
import jurisk.algorithms.pathfinding.AStar.aStar
import jurisk.utils.Bounded

object Dijkstra {
  def dijkstra[N, C: Numeric: Ordering: Bounded](
    start: N,
    successors: N => List[(N, C)],
    success: N => Boolean,
  ): Option[(NonEmptyList[N], C)] = {
    val Zero = implicitly[Numeric[C]].zero
    aStar[N, C](start, successors, _ => Zero, success)
  }
}
