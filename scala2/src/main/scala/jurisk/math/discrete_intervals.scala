package jurisk.math

import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

final case class NonOverlappingDiscreteIntervalSet[N: Numeric](
  data: Set[InclusiveDiscreteInterval[N]]
) {
  def add(
    interval: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    NonOverlappingDiscreteIntervalSet[N](
      if (data.isEmpty) {
        Set(interval)
      } else {
        data.flatMap(_.add(interval))
      }
    )

  def subtract(
    interval: InclusiveDiscreteInterval[N]
  ): NonOverlappingDiscreteIntervalSet[N] =
    NonOverlappingDiscreteIntervalSet[N](
      data.flatMap(_.subtract(interval))
    )

  def minUnsafe: N =
    data.map(_.from).min

  def size: N = data.toList.map(_.size).sum

  def toSet: Set[N] = data.flatMap(_.toList)
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
  private val numericN: Numeric[N] = implicitly[Numeric[N]]
  private val One: N               = numericN.one

  def size: N = to - from + One

  def toList: List[N] = from to to

  def add(
    other: InclusiveDiscreteInterval[N]
  ): Set[InclusiveDiscreteInterval[N]] =
    if (other.from > to || other.to < from) Set(this, other)
    else
      Set(
        InclusiveDiscreteInterval(
          numericN.min(this.from, other.from),
          numericN.max(this.to, other.to),
        )
      )

  def subtract(
    other: InclusiveDiscreteInterval[N]
  ): Set[InclusiveDiscreteInterval[N]] =
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
