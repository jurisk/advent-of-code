package jurisk.algorithms

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.mutable

package object pathfinding {
  @tailrec
  private[pathfinding] def reassemblePath[N](
    start: N,
    parents: mutable.Map[N, N],
    current: N,
    acc: List[N],
  ): NonEmptyList[N] =
    if (current == start) NonEmptyList(current, acc)
    else reassemblePath(start, parents, parents(current), current :: acc)

}
