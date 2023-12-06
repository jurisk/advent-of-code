package jurisk.math

import cats.implicits._
import jurisk.math.Enumerated.EnumeratedOps

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

/** Both `from` and `to` are inclusive.
  */
final case class DiscreteInterval[N: Numeric: Enumerated](
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
    other: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] =
    if (other.from > to + One || other.to < from - One)
      new DiscreteIntervalSet(Set(this, other))
    else
      new DiscreteIntervalSet(
        Set(
          DiscreteInterval(
            numericN.min(this.from, other.from),
            numericN.max(this.to, other.to),
          )
        )
      )

  def intersect(
    other: DiscreteInterval[N]
  ): Option[DiscreteInterval[N]] =
    if (other.from > to || other.to < from)
      none
    else
      DiscreteInterval(from max other.from, to min other.to).some

  def subtract(
    other: DiscreteInterval[N]
  ): DiscreteIntervalSet[N] = {
    val set = if (other.from > to || other.to < from) {
      Set(this)
    } else if (other.from <= from && other.to >= to) {
      Set.empty[DiscreteInterval[N]]
    } else if (other.from > from && other.to < to) {
      Set(
        DiscreteInterval[N](from, other.from - One),
        DiscreteInterval[N](other.to + One, to),
      )
    } else if (other.from <= from) { // && other.to < to
      Set(
        DiscreteInterval[N](other.to + One, to)
      )
    } else { // other.from > from) && (other.to >= to)
      Set(
        DiscreteInterval[N](from, other.from - One)
      )
    }

    new DiscreteIntervalSet(set)
  }
}
