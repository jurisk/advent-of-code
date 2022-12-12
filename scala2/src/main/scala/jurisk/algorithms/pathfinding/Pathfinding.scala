package jurisk.algorithms.pathfinding

import scala.annotation.tailrec
import scala.collection.mutable

object Pathfinding {
  @tailrec
  private def reassemblePath[N](
    start: N,
    parents: mutable.Map[N, N],
    current: N,
    acc: List[N],
  ): List[N] = {
    val newAcc = current :: acc

    if (current == start) newAcc
    else reassemblePath(start, parents, parents(current), newAcc)
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
  ): Option[List[N]] = {
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

  def dfsLength[N](
                    start: N,
                    successors: N => List[N],
                    isGoal: N => Boolean,
                  ): Option[Int] =
    dfs(start, successors, isGoal).map(_.length - 1)

  /** https://en.wikipedia.org/wiki/Depth-first_search */
  def dfs[N](
    start: N,
    successors: N => List[N],
    isGoal: N => Boolean,
  ): Option[List[N]] = {
    val visited: mutable.Set[N]    = mutable.Set()
    val stack: mutable.Stack[N]    = mutable.Stack()
    val parents: mutable.Map[N, N] = mutable.Map()

    stack.push(start)

    while (stack.nonEmpty) {
      val next = stack.pop()
      if (isGoal(next)) {
        return Some(reassemblePath(start, parents, next, Nil))
      } else {
        if (!visited.contains(next)) {
          visited.add(next)
          successors(next) foreach { successor =>
            parents.put(successor, next)
            stack.push(successor)
          }
        }
      }
    }

    None
  }
}
