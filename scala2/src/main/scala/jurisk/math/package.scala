package jurisk

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

package object math {
  implicit class IntOps(n: Int) {
    def parity: Int           = n % 2
    def halfRoundingUp: Int   = n - halfRoundingDown
    def halfRoundingDown: Int = n / 2
  }

  implicit class LongOps(n: Long) {
    def parity: Long           = n % 2
    def halfRoundingUp: Long   = n - halfRoundingDown
    def halfRoundingDown: Long = n / 2
  }

  def absForWrappingAround[N: Integral](x: N, y: N): N =
    ((x % y) + y) % y

  def lcm[N: Integral](a: N, b: N): N = {
    val Zero = implicitly[Integral[N]].zero

    (a.abs, b.abs) match {
      case (0, _) | (_, 0) => Zero
      case (m, n)          => m * n / gcd(a, b)
    }
  }

  def lcmMany[N: Integral](numbers: Iterable[N]): N = {
    val One = implicitly[Integral[N]].one

    numbers.foldLeft(One) { (a, b) =>
      (a / gcd(a, b)) * b
    }
  }

  @tailrec
  def gcd[N: Integral](a: N, b: N): N = {
    val Zero = implicitly[Integral[N]].zero

    (a.abs, b.abs) match {
      case (0, 0) => Zero
      case (m, 0) => m
      case (0, n) => n
      case (m, n) => gcd(n, m % n)
    }
  }

  def pow(a: Int, b: Int): Long =
    Math.pow(a, b).toLong

  def positiveDivisors(n: Long): Seq[Long] =
    (1L to Math.sqrt(n.toDouble).toLong)
      .flatMap(i =>
        if (n % i == 0) {
          if (i * i == n) {
            // Avoid duplicates when n is a perfect square
            Seq(i)
          } else {
            Seq(i, n / i)
          }
        } else Seq.empty[Long]
      )
      .sorted

  def positiveAndNegativeDivisors(n: Long): Seq[Long] = {
    val positive = positiveDivisors(n.abs)
    positive.map(-_).reverse ++ positive
  }

  def countLongsBetweenExclusive(low: Double, high: Double): Long = {
    def floorExclusive(x: Double): Long = {
      val result = x.floor
      if (result == x) {
        result.toLong - 1
      } else {
        result.toLong
      }
    }

    def ceilExclusive(x: Double): Long = {
      val result = x.ceil
      if (result == x) {
        result.toLong + 1
      } else {
        result.toLong
      }
    }

    floorExclusive(high) - ceilExclusive(low) + 1
  }
}
