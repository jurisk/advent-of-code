package jurisk

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.abs

package object math {
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
