package jurisk.algorithms.pathfinding

import jurisk.algorithms.pathfinding.Pathfinding.{bfs, dfs, shortestPath}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class PathfindingSpec extends AnyFlatSpec {
  "Shortest path" should "find shortest path" in {
    shortestPath[Int](
      0,
      x => List(x + 1, x * 2, x * 3, x * 7),
      _ == 1337,
    ).map(_.length) shouldEqual Some(9)
  }

  "BFS" should "find shortest path" in {
    bfs[Int](
      0,
      x => List(x + 1, x * 2, x * 3, x * 7),
      _ == 1337,
    ).map(_.length) shouldEqual Some(9)
  }

  "DFS" should "find path" in {
    dfs[Int](
      1,
      x => if (x > 1000) Nil else List(x * 2, x * 3, x * 5, x * 7),
      _ == 210,
    ).map(_.length) shouldEqual Some(5)
  }
}
