package jurisk.utils

import cats.Eq
import cats.implicits._
import jurisk.utils.Parsing.StringOps

import scala.math.Integral.Implicits.infixIntegralOps

object CollectionOps {
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
}
