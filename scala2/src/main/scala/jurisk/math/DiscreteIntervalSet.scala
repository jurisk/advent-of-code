package jurisk.math

import cats.implicits._
import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

final case class DiscreteIntervalSet[N: Numeric](
  private val set: Set[DiscreteInterval[N]]
) {
  def intervals: Seq[DiscreteInterval[N]] = set.toSeq

  def union(
    other: DiscreteIntervalSet[N]
  ): DiscreteIntervalSet[N] =
    other.intervals.foldLeft(this) { case (acc, x) =>
      acc.add(x)
    }

  def add(
    interval: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] =
    if (isEmpty) {
      new DiscreteIntervalSet[N](Set(interval))
    } else {
      // TODO: This is not really correct, see https://github.com/jurisk/leetcode/blob/master/rust/src/solution/p0057_insert_interval.rs
      new DiscreteIntervalSet[N](
        intervals.flatMap(x => (x union interval).intervals).toSet
      )
    }

  def subtract(
    interval: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] =
    DiscreteIntervalSet[N](
      intervals.flatMap(_.subtract(interval).intervals)
    )

  def subtract(
    other: DiscreteIntervalSet[N]
  ): DiscreteIntervalSet[N] =
    other.intervals.foldLeft(this) { case (acc, x) =>
      acc subtract x
    }

  def size: N          = intervals.toList.map(_.size).sum
  def isEmpty: Boolean = set.isEmpty

  def valuesSet: Set[N] = intervals.flatMap(_.toList).toSet

  def minOption: Option[N] = intervals.map(_.from).minOption
  def maxOption: Option[N] = intervals.map(_.to).maxOption
}

object DiscreteIntervalSet {
  def apply[N: Numeric](
    input: Seq[DiscreteInterval[N]]
  ): DiscreteIntervalSet[N] =
    input.foldLeft(DiscreteIntervalSet.empty[N]) { case (acc, x) =>
      acc add x
    }

  def fromInclusiveInterval[N: Numeric: Enumerated](
    from: N,
    to: N,
  ): DiscreteIntervalSet[N] =
    DiscreteIntervalSet[N](
      Set(DiscreteInterval[N](from, to))
    )

  def empty[N: Numeric]: DiscreteIntervalSet[N] =
    new DiscreteIntervalSet[N](Set.empty)
}
