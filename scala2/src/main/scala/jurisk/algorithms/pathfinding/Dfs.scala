package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList

import scala.collection.mutable

object Dfs {
  def dfsVisitAll[N](
    start: N,
    successors: N => List[N],
    visit: N => Unit,
  ): Unit = {
    val visited: mutable.Set[N] = mutable.Set()
    val stack: mutable.Stack[N] = mutable.Stack()
    val parents: mutable.Map[N, N] = mutable.Map()

    stack.push(start)

    while (stack.nonEmpty) {
      val next = stack.pop()
      visit(next)
      if (!visited.contains(next)) {
        visited.add(next)
        successors(next) foreach { successor =>
          parents.put(successor, next)
          stack.push(successor)
        }
      }
    }
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
  ): Option[NonEmptyList[N]] = {
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
