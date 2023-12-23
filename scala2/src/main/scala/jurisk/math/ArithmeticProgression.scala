package jurisk.math

// 1-based indexing, like in https://en.wikipedia.org/wiki/Arithmetic_progression
final case class ArithmeticProgression(
  a_1: Long,
  d: Long,
) {
  def a_n(n: Long): Long = a_1 + (n - 1) * d
  def S_n(n: Long): Long = (n * (a_1 + a_n(n))) / 2
}
