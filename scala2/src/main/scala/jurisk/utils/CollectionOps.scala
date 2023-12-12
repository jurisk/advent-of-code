package jurisk.utils

import cats.Eq
import cats.implicits._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

object CollectionOps {
  implicit class ListOps[T](list: List[T]) {
    def splitBySeparator(separator: T): List[List[T]] = {
      @tailrec
      def f(
        remaining: List[T],
        current: List[T],
        acc: List[List[T]],
      ): List[List[T]] =
        remaining match {
          case Nil                 =>
            val finalAccumulator = if (current.nonEmpty) current :: acc else acc
            finalAccumulator.reverse.map(_.reverse)
          case `separator` :: tail =>
            val newAcc = if (current.nonEmpty) current :: acc else acc
            f(tail, Nil, newAcc)
          case head :: tail        =>
            f(tail, head :: current, acc)
        }

      f(list, Nil, Nil)
    }

    def multiplyAndFlattenWithSeparator(times: Int, separator: T): List[T] = {
      val lists = List.fill(times)(list)
      if (lists.isEmpty) {
        Nil
      } else {
        lists.reduce[List[T]] { case (a, b) =>
          a ::: List(separator) ::: b
        }
      }
    }

    def multiplyAndFlatten(times: Int): List[T] =
      List.fill(times)(list).flatten
  }

  implicit class IterableOps[T](seq: Iterable[T]) {
    def counts: Map[T, Int] = seq.groupMapReduce(identity)(_ => 1)(_ + _)

    def allDistinct: Boolean = seq.toSet.size == seq.size

    def singleElementUnsafe: T =
      if (seq.size == 1) seq.head
      else
        s"Expected a single element, but got ${seq.toList.mkString("(", ", ", ")")}".fail

    // We just keep mistyping this so much, we may as well add it :shrug:
    def singleResultUnsafe: T = singleElementUnsafe

    def twoElementsUnsafe: (T, T) =
      if (seq.size == 2) (seq.head, seq.tail.head)
      else s"Expected two elements, but got $seq".fail
  }

  implicit class EqIterableOps[T: Eq](seq: Iterable[T]) {
    def allEqual(value: T): Boolean =
      seq.forall(_ === value)
  }

  implicit class IntegralSeqOps[N: Integral](seq: Seq[N]) {
    def differences: Seq[N] =
      seq.sliding2 map { case (a, b) =>
        b - a
      }
  }

  implicit class IndexedSeqOps[T](seq: IndexedSeq[T]) {
    def atIndexWithWraparound[N: Integral](index: N): T = {
      val I        = implicitly[Integral[N]]
      val adjusted = index % I.fromInt(seq.length)
      seq(adjusted.toInt)
    }
  }

  implicit class ListListOps[T](listList: List[List[T]]) {
    def dropFromFirstEliminatingEmpty(n: Int): List[List[T]] =
      listList match {
        case head :: tail =>
          val updated = head.drop(n)
          if (updated.isEmpty) {
            tail
          } else {
            updated :: tail
          }
        case Nil          => Nil
      }
  }
}
