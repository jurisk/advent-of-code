package jurisk.adventofcode.y2024

import jurisk.math.pow
import jurisk.utils.Memoize
import mouse.all.booleanSyntaxMouse

import scala.math.log10

object Advent11 {
  type Input = Vector[Long]
  type N     = Long

  private def halves(n: Long): Option[(Long, Long)] = {
    val digits = log10(n.doubleValue).toInt + 1

    (digits % 2 == 0).option {
      val halfLength = digits / 2
      val divisor    = pow(10, halfLength)
      val left       = n / divisor
      val right      = n % divisor
      (left, right)
    }
  }

  def blink(data: Input): Input =
    data.flatMap { n =>
      if (n == 0) {
        Vector(1)
      } else {
        halves(n) match {
          case Some((left, right)) =>
            Vector(left, right)
          case None                =>
            Vector(n * 2024)
        }
      }
    }

  private val memoizedSolve: (Long, Int) => N   = Memoize.memoize2(solve)
  private def solve(n: Long, blinks: Int): Long =
    if (blinks <= 0) {
      1
    } else {
      if (n == 0) {
        memoizedSolve(1, blinks - 1)
      } else {
        halves(n) match {
          case Some((left, right)) =>
            memoizedSolve(left, blinks - 1) + memoizedSolve(right, blinks - 1)
          case None                =>
            memoizedSolve(n * 2024, blinks - 1)
        }
      }
    }

  def blinkNTimes(data: Input, blinks: Int): Input =
    (0 until blinks).foldLeft(data)((acc, _) => blink(acc))

  def part1(data: Input, blinks: Int = 25): N =
    blinkNTimes(data, blinks).size

  def part2(data: Input, blinks: Int = 75): N =
    data.map(n => memoizedSolve(n, blinks)).sum

  val realData: Input = Vector(6563348, 67, 395, 0, 6, 4425, 89567, 739318)

  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
