package jurisk.math

import cats.implicits._
import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

final case class NonOverlappingDiscreteIntervalSet[N: Numeric](
  data: Set[InclusiveDiscreteInterval[N]]
) {
  def union(
    other: NonOverlappingDiscreteIntervalSet[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    other.data.foldLeft(this) { case (acc, x) =>
      acc.add(x)
    }

  def add(
    interval: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    if (data.isEmpty) {
      NonOverlappingDiscreteIntervalSet[N](Set(interval))
    } else {
      NonOverlappingDiscreteIntervalSet[N](
        data.flatMap(x => (x union interval).data)
      )
    }

  def subtract(
    interval: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    NonOverlappingDiscreteIntervalSet[N](
      data.flatMap(_.subtract(interval).data)
    )

  def subtract(
    other: NonOverlappingDiscreteIntervalSet[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    other.data.foldLeft(this) { case (acc, x) =>
      acc subtract x
    }

  def minUnsafe: N =
    data.map(_.from).min

  def size: N = data.toList.map(_.size).sum

  def toSet: Set[N] = data.flatMap(_.toList)

  def minOption: Option[N] = data.map(_.from).minOption
  def maxOption: Option[N] = data.map(_.to).maxOption
}

object NonOverlappingDiscreteIntervalSet {
  def createInclusive[N: Numeric: Enumerated](
    from: N,
    to: N,
  ): NonOverlappingDiscreteIntervalSet[N] =
    NonOverlappingDiscreteIntervalSet[N](
      Set(InclusiveDiscreteInterval[N](from, to))
    )

  def empty[N: Numeric]: NonOverlappingDiscreteIntervalSet[N] =
    NonOverlappingDiscreteIntervalSet[N](
      Set.empty
    )
}

final case class InclusiveDiscreteInterval[N: Numeric: Enumerated](
  from: N,
  to: N,
) {
  assert(from <= to, "From must be <= than to")

  private val numericN: Numeric[N] = implicitly[Numeric[N]]
  private val One: N               = numericN.one

  def contains(value: N): Boolean = value >= from && value <= to

  def size: N = to - from + One

  def toList: List[N] = from to to

  def union(
    other: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    if (other.from > to + One || other.to < from - One)
      NonOverlappingDiscreteIntervalSet(Set(this, other))
    else
      NonOverlappingDiscreteIntervalSet(
        Set(
          InclusiveDiscreteInterval(
            numericN.min(this.from, other.from),
            numericN.max(this.to, other.to),
          )
        )
      )

  def intersect(
    other: InclusiveDiscreteInterval[N]
  ): Option[InclusiveDiscreteInterval[N]] =
    if (other.from > to || other.to < from)
      none
    else
      InclusiveDiscreteInterval(from max other.from, to min other.to).some

  def subtract(
    other: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] = NonOverlappingDiscreteIntervalSet {
    if (other.from > to || other.to < from) {
      Set(this)
    } else if (other.from <= from && other.to >= to) {
      Set.empty
    } else if (other.from > from && other.to < to) {
      Set(
        InclusiveDiscreteInterval[N](from, other.from - One),
        InclusiveDiscreteInterval[N](other.to + One, to),
      )
    } else if (other.from <= from) { // && other.to < to
      Set(
        InclusiveDiscreteInterval[N](other.to + One, to)
      )
    } else { // other.from > from) && (other.to >= to)
      Set(
        InclusiveDiscreteInterval[N](from, other.from - One)
      )
    }
  }
}
