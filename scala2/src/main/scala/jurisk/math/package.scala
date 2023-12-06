package jurisk

import scala.annotation.tailrec
import scala.math.abs

package object math {
  def absForWrappingAround(x: Int, y: Int): Int =
    ((x % y) + y) % y

  def lcm(a: Int, b: Int): Int =
    (abs(a), abs(b)) match {
      case (0, _) | (_, 0) => 0
      case (m, n)          => m * n / gcd(a, b)
    }

  @tailrec
  def gcd(a: Int, b: Int): Int =
    (abs(a), abs(b)) match {
      case (0, 0) => 0
      case (m, 0) => m
      case (0, n) => n
      case (m, n) => gcd(n, m % n)
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
