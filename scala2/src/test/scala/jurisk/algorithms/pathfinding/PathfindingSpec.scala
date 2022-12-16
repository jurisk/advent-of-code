package jurisk.algorithms.pathfinding

import jurisk.algorithms.pathfinding.Pathfinding.shortestPath
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
    Bfs
      .bfs[Int](
        0,
        x => List(x + 1, x * 2, x * 3, x * 7),
        _ == 1337,
      )
      .map(_.length) shouldEqual Some(9)
  }

  "DFS" should "find path" in {
    Dfs
      .dfs[Int](
        1,
        x => if (x > 1000) Nil else List(x * 2, x * 3, x * 5, x * 7),
        _ == 210,
      )
      .map(_.length) shouldEqual Some(5)
  }

  "Dijkstra all" should "find all paths" in {
    def dist(n: Int): Int = (n % 3) + 1

    val result   = Dijkstra.dijkstraAll(
      1,
      (x: Int) =>
        if (x > 1000) Nil else List(2, 3, 5, 7).map(n => (x * n, dist(n))),
    )
    result.size shouldEqual 277
    val testCase = List(2, 2, 3, 5, 7, 7)
    result(testCase.product)._2 shouldEqual testCase.map(dist).sum
  }
}
