package jurisk.algorithms.pathfinding

object ConnectedComponents {
  def connectedComponents[N](
    starts: IterableOnce[N],
    successors: N => IterableOnce[N],
  ): Set[Set[N]] = {
    var results: Set[Set[N]] = Set.empty
    var visited: Set[N]      = Set.empty

    starts.iterator foreach { n =>
      if (!visited.contains(n)) {
        val island = Bfs.bfsReachable(n, successors).toSet
        results += island
        visited = visited ++ island
      }
    }

    results
  }
}
