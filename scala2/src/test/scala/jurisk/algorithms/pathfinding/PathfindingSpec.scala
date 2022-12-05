package jurisk.algorithms.pathfinding

import jurisk.algorithms.pathfinding.Pathfinding.{bfs, dfs}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class PathfindingSpec extends AnyFlatSpec {
  "BFS" should "find shortest path" in {
    bfs[Int](
      0,
      x => List(x + 1, x * 2, x * 3, x * 7),
      _ == 1337,
    ) shouldEqual Some(List(0, 1, 3, 9, 27, 189, 190, 191, 1337))
  }

  "DFS" should "find path" in {
    dfs[Int](
      1,
      x => if (x > 1000) Nil else List(x * 2, x * 3, x * 5, x * 7),
      _ == 210,
    ).map(_.length) shouldEqual Some(5)
  }
}
