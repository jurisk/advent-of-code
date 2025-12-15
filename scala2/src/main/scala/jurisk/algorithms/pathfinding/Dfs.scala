package jurisk.algorithms.pathfinding

import cats.data.NonEmptyList

import scala.collection.mutable

object Dfs {
  def dfsVisitAllNoMemory[N](
    start: N,
    successors: N => IterableOnce[N],
    visit: N => Unit,
  ): Unit = {
    val stack: mutable.Stack[N]    = mutable.Stack()
    val parents: mutable.Map[N, N] = mutable.Map()

    stack.push(start)

    while (stack.nonEmpty) {
      val next = stack.pop()
      visit(next)
      successors(next).iterator foreach { successor =>
        parents.put(successor, next)
        stack.push(successor)
      }
    }
  }

  def dfsVisitAll[N](
    start: N,
    successors: N => IterableOnce[N],
    visit: N => Unit,
  ): Unit = {
    val visited: mutable.Set[N]    = mutable.Set()
    val stack: mutable.Stack[N]    = mutable.Stack()
    val parents: mutable.Map[N, N] = mutable.Map()

    stack.push(start)

    while (stack.nonEmpty) {
      val next = stack.pop()
      visit(next)
      if (!visited.contains(next)) {
        visited.add(next)
        successors(next).iterator foreach { successor =>
          parents.put(successor, next)
          stack.push(successor)
        }
      }
    }
  }

  def dfsAllPaths[N](
    start: N,
    successors: N => IterableOnce[N],
    isGoal: N => Boolean,
    visit: List[N] => Unit,
  ): Unit = {
    def backtrack(
      current: N,
      visited: Set[N],
      path: List[N],
    ): Unit =
      if (!visited.contains(current)) {
        val newPath = current :: path
        if (isGoal(current)) {
          visit(newPath.reverse)
        } else {
          val newVisited = visited + current
          successors(current).iterator.foreach { successor =>
            backtrack(successor, newVisited, newPath)
          }
        }
      }

    backtrack(start, Set.empty, Nil)
  }

  def dfsLength[N](
    start: N,
    successors: N => IterableOnce[N],
    isGoal: N => Boolean,
  ): Option[Int] =
    dfs(start, successors, isGoal).map(_.length - 1)

  /** https://en.wikipedia.org/wiki/Depth-first_search */
  def dfs[N](
    start: N,
    successors: N => IterableOnce[N],
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
          successors(next).iterator foreach { successor =>
            parents.put(successor, next)
            stack.push(successor)
          }
        }
      }
    }

    None
  }
}
