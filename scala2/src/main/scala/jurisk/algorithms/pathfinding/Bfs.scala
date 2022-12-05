package jurisk.algorithms.pathfinding


import scala.annotation.tailrec
import scala.collection.mutable

object Bfs {
  /** https://en.wikipedia.org/wiki/Breadth-first_search */
  def bfs[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[List[N]] = {
    val visited: mutable.Set[N]    = mutable.Set()
    val parents: mutable.Map[N, N] = mutable.Map()
    val queue: mutable.Queue[N]    = mutable.Queue()

    @tailrec
    def reassemble(current: N, acc: List[N]): List[N] = {
      val newAcc = current :: acc

      if (current == start) newAcc
      else reassemble(parents(current), newAcc)
    }

    queue.enqueue(start)
    visited.add(start)

    while (queue.nonEmpty) {
      val next = queue.dequeue();
      if (isGoal(next)) {
        return Some(reassemble(next, Nil))
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
