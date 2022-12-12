package jurisk.utils

trait Bounded[A] {
  def minValue: A
  def maxValue: A
}

object Bounded {
  def apply[A](min: A, max: A): Bounded[A] = new Bounded[A] {
    def minValue: A = min
    def maxValue: A = max
  }

  implicit val intBounded: Bounded[Int] = Bounded(Int.MinValue, Int.MaxValue)
  implicit val longBounded: Bounded[Long] = Bounded(Long.MinValue, Long.MaxValue)
}