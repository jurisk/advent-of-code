package jurisk.math

trait Enumerated[N] {
  def to(fromInclusive: N, toInclusive: N): Seq[N]
}

object Enumerated {
  implicit class EnumeratedOps[N](val fromInclusive: N)(implicit
    ev: Enumerated[N]
  ) {
    def to(toInclusive: N): Seq[N] = ev.to(fromInclusive, toInclusive)
  }

  implicit object IntEnumerated extends Enumerated[Int] {
    def to(fromInclusive: Int, toInclusive: Int): Seq[Int] =
      fromInclusive to toInclusive
  }

  implicit object LongEnumerated extends Enumerated[Long] {
    def to(fromInclusive: Long, toInclusive: Long): Seq[Long] =
      fromInclusive to toInclusive
  }
}
