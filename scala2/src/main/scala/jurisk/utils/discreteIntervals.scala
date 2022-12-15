package jurisk.utils

final case class NonOverlappingDiscreteIntervalSet(
  data: Set[InclusiveDiscreteInterval],
) {
  def subtract(interval: InclusiveDiscreteInterval): NonOverlappingDiscreteIntervalSet = NonOverlappingDiscreteIntervalSet(
    data.flatMap(_.subtract(interval))
  )

  def size: Int = data.map(_.size).sum

  def values: Set[Int] = data.flatMap(_.values)
}

object NonOverlappingDiscreteIntervalSet {
  def createInclusive(from: Int, to: Int): NonOverlappingDiscreteIntervalSet = NonOverlappingDiscreteIntervalSet(
    Set(InclusiveDiscreteInterval(from, to))
  )
}

final case class InclusiveDiscreteInterval(from: Int, to: Int) {
  def size: Int = to - from + 1

  def values: List[Int] = (from to to).toList

  def subtract(other: InclusiveDiscreteInterval): Set[InclusiveDiscreteInterval] = {
    if (other.from > to || other.to < from) {
      Set(this)
    } else if (other.from <= from && other.to >= to) {
      Set.empty
    } else if (other.from > from && other.to < to) {
      Set(
        InclusiveDiscreteInterval(from, other.from - 1),
        InclusiveDiscreteInterval(other.to + 1, to),
      )
    } else if (other.from <= from) { // && other.to < to
      Set(
        InclusiveDiscreteInterval(other.to + 1, to)
      )
    } else { // other.from > from) && (other.to >= to)
      Set(
        InclusiveDiscreteInterval(from, other.from - 1)
      )
    }
  }
}