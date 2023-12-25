package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList

import scala.collection.mutable

object Bfs {
  def bfsReachable[N](
    start: N,
    successors: N => Seq[N],
  ): List[N] = {
    var results: Vector[N] = Vector.empty

    bfsVisitAll[N](start, successors, n => results = results.appended(n))

    results.toList
  }

  def bfsVisitAll[N](
    start: N,
    successors: N => Seq[N],
    visit: N => Unit,
  ): Unit = {
    val visited: mutable.Set[N] = mutable.Set()
    val queue: mutable.Queue[N] = mutable.Queue()

    queue.enqueue(start)
    visited.add(start)

    while (queue.nonEmpty) {
      val next = queue.dequeue()
      visit(next)
      successors(next) foreach { successor =>
        if (!visited.contains(successor)) {
          visited.add(successor)
          queue.enqueue(successor)
        }
      }
    }
  }

  def bfsLength[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[Int] =
    bfs(start, successors, isGoal).map(_.length - 1)

  /** https://en.wikipedia.org/wiki/Breadth-first_search */
  def bfs[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[NonEmptyList[N]] = {
    val visited: mutable.Set[N]    = mutable.Set()
    val queue: mutable.Queue[N]    = mutable.Queue()
    val parents: mutable.Map[N, N] = mutable.Map()

    queue.enqueue(start)
    visited.add(start)

    while (queue.nonEmpty) {
      val next = queue.dequeue()
      if (isGoal(next)) {
        return Some(reassemblePath(start, parents, next, Nil))
      } else {
        successors(next) foreach { successor =>
          if (!visited.contains(successor)) {
            visited.add(successor)
            parents.put(successor, next)
            queue.enqueue(successor)
          }
        }
      }
    }

    None
  }
}
