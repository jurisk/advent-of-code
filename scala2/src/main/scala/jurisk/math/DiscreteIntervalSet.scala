package jurisk.math

import cats.implicits._
import scala.math.Ordering.Implicits._

final case class DiscreteIntervalSet[N: Numeric](
  intervals: Vector[DiscreteInterval[N]]
) {
  def union(
    other: DiscreteIntervalSet[N]
  ): DiscreteIntervalSet[N] =
    other.intervals.foldLeft(this) { case (acc, x) =>
      acc.add(x)
    }

  def add(
    interval: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] = {
    var results                           = Vector.empty[DiscreteInterval[N]]
    var open: Option[DiscreteInterval[N]] = interval.some

    intervals foreach { interval =>
      open match {
        case Some(open_interval) =>
          val union = (interval union open_interval).intervals
          if (union.length == 1) { // overlaps or touches
            open = union.headOption
          } else {
            if (open_interval.from < interval.from) {
              results = results :+ open_interval
              open = None
            }

            results = results :+ interval
          }
        case None                =>
          results = results :+ interval
      }
    }

    open foreach { open =>
      results = results :+ open
    }

    new DiscreteIntervalSet[N](results)
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

  def size: N          = intervals.map(_.size).sum
  def isEmpty: Boolean = intervals.isEmpty

  def valuesSet: Set[N] = intervals.flatMap(_.toList).toSet

  def minOption: Option[N] = intervals.map(_.from).minOption
  def maxOption: Option[N] = intervals.map(_.to).maxOption
}

object DiscreteIntervalSet {
  def continuous[N: Numeric](
    interval: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] = new DiscreteIntervalSet[N](Vector(interval))

  def apply[N: Numeric](
    head: DiscreteInterval[N],
    tail: DiscreteInterval[N]*
  ): DiscreteIntervalSet[N] =
    DiscreteIntervalSet[N](head +: tail)

  private def apply[N: Numeric](
    input: Seq[DiscreteInterval[N]]
  ): DiscreteIntervalSet[N] =
    input.foldLeft(DiscreteIntervalSet.empty[N]) { case (acc, x) =>
      acc add x
    }

  def fromInterval[N: Numeric: Enumerated](
    from: N,
    to: N,
  ): DiscreteIntervalSet[N] =
    DiscreteIntervalSet.continuous[N](DiscreteInterval[N](from, to))

  def empty[N: Numeric]: DiscreteIntervalSet[N] =
    new DiscreteIntervalSet[N](Vector.empty)
}
