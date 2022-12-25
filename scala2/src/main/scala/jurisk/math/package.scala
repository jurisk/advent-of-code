package jurisk

import scala.annotation.tailrec

package object math {
  def absForWrappingAround(x: Int, y: Int): Int = {
    ((x % y) + y) % y
  }

  def lcm(a: Int, b: Int): Int = if (a == 0 || b == 0) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def pow(a: Int, b: Int): Long = {
    Math.pow(a, b).toLong
  }
}
