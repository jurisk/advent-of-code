package jurisk.adventofcode.y2023

import scala.annotation.tailrec

object Sort {
  def consideringEqualValues[T: Ordering](set: Set[T]): List[Set[T]] = {
    val sorted = set.toList.sorted

    @tailrec
    def f(remaining: List[T], acc: List[Set[T]]): List[Set[T]] =
      remaining match {
        case x :: xs =>
          val others = xs.takeWhile { y =>
            Ordering[T].equiv(x, y)
          }

          val batch     = Set(x) ++ others.toSet
          val remaining = xs.drop(others.size)
          f(remaining, batch :: acc)

        case Nil => acc.reverse
      }

    f(sorted, Nil)
  }
}
