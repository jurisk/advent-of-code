package jurisk.math

trait Enumerated[N] {
  def to(fromInclusive: N, toInclusive: N): List[N]
}

object Enumerated {
  implicit class EnumeratedOps[N](val fromInclusive: N)(implicit
    ev: Enumerated[N]
  ) {
    def to(toInclusive: N): List[N] = ev.to(fromInclusive, toInclusive)
  }

  implicit object IntEnumerated extends Enumerated[Int] {
    def to(fromInclusive: Int, toInclusive: Int): List[Int] =
      (fromInclusive to toInclusive).toList
  }

  implicit object LongEnumerated extends Enumerated[Long] {
    def to(fromInclusive: Long, toInclusive: Long): List[Long] =
      (fromInclusive to toInclusive).toList
  }
}
