package jurisk.utils

import cats.Eq
import cats.implicits._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
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

  implicit class SeqOps[T](seq: Seq[T]) {
    private val NotFound = -1

    def firstIndexOf(value: T): Option[Int] =
      seq.indexOf(value).some.filter(_ != NotFound)

    def firstIndexWhere(p: T => Boolean, from: Int = 0): Option[Int] = {
      val result = seq.indexWhere(p, from)
      if (result == -1) {
        none
      } else {
        result.some
      }
    }
  }

  implicit class IterableOps[T](iterable: Iterable[T]) {
    def counts: Map[T, Int] = iterable.groupMapReduce(identity)(_ => 1)(_ + _)

    def consecutiveGroupCounts: List[(T, Int)] =
      iterable.headOption match {
        case Some(head) =>
          val (taken, remaining) = iterable.span(_ == head)
          (head -> taken.size) :: remaining.consecutiveGroupCounts

        case None =>
          Nil
      }

    def allDistinct: Boolean = iterable.toSet.size == iterable.size

    def singleElementUnsafe: T =
      if (iterable.size == 1) iterable.head
      else
        s"Expected a single element, but got ${iterable.toList.mkString("(", ", ", ")")}".fail

    // We just keep mistyping this so much, we may as well add it :shrug:
    def singleResultUnsafe: T = singleElementUnsafe

    def twoElementsUnsafe: (T, T) =
      if (iterable.size == 2) (iterable.head, iterable.tail.head)
      else s"Expected two elements, but got $iterable".fail
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

  implicit class ArraySeqOps[T](seq: ArraySeq[T]) {
    def updatedWith(index: Int)(modify: T => T): ArraySeq[T] =
      seq.lift(index) match {
        case Some(currentValue) => seq.updated(index, modify(currentValue))
        case None               => seq
      }
  }

  implicit class IndexedSeqOps[T](seq: IndexedSeq[T]) {
    def atIndexWithWraparound[N: Integral](index: N): T = {
      val I        = implicitly[Integral[N]]
      val adjusted = index % I.fromInt(seq.length)
      seq(adjusted.toInt)
    }

    def centerElementUnsafe: T = {
      assert(seq.length % 2 == 1)
      seq(seq.length / 2)
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

  implicit class VectorOps[T](private val vector: Vector[T]) extends AnyVal {
    def updatedWith(index: Int)(modify: T => T): Vector[T] =
      vector.lift(index) match {
        case Some(currentValue) => vector.updated(index, modify(currentValue))
        case None               => vector
      }

    def removeAt(index: Int): Vector[T] =
      vector.slice(0, index) ++ vector.slice(index + 1, vector.length)

    def insertAt(index: Int, slice: Vector[T]): Vector[T] =
      vector.slice(0, index) ++ slice ++ vector.slice(index, vector.length)

    def replaceAt(index: Int, slice: Vector[T]): Vector[T] =
      vector.slice(0, index) ++ slice ++ vector.slice(index + 1, vector.length)
  }

  implicit class OptionOps[T](private val option: Option[T]) extends AnyVal {
    def orFail(message: => String): T =
      option.getOrElse(message.fail)
  }
}
