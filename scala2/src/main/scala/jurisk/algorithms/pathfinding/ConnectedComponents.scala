package jurisk.algorithms.pathfinding

object ConnectedComponents {
  def connectedComponents[N](
    starts: List[N],
    successors: N => List[N],
  ): Set[Set[N]] = {
    var results: Set[Set[N]] = Set.empty
    var visited: Set[N]      = Set.empty

    starts foreach { n =>
      if (!visited.contains(n)) {
        val island = Bfs.bfsReachable(n, successors).toSet
        results += island
        visited = visited ++ island
      }
    }

    results
  }
}
