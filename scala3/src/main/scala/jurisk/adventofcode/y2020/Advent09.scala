package jurisk.adventofcode.y2020

import cats.implicits.*
import AdventApp.ErrorMessage

import scala.annotation.tailrec

object Advent09 extends SingleLineAdventApp[Long, Long]:
  def fileName: String = "09.txt"

  extension [T] (self: Boolean)
    def option(x: => T): Option[T] = if (self) x.some else none

  def solution1(testCases: List[Long]): Long =
    solution1(testCases, 25).getOrElse(sys.error("Failed to solve"))

  def solution1(testCases: List[Long], preambleSize: Int): Option[Long] =
    @tailrec
    def f(preamble: Vector[Long], remaining: List[Long]): Option[Long] =
      def isSumOfTwo(x: Long): Boolean = preamble.combinations(2).exists(_.sum == x)

      remaining match
        case Nil => None
        case x :: xs => if isSumOfTwo(x) then f(preamble.tail :+ x, xs) else x.some

    val (a, b) = testCases.splitAt(preambleSize)
    f(a.toVector, b)

  def solution2(testCases: List[Long]): Long =
    solution2(testCases, 25).getOrElse(sys.error("Failed to solve"))

  def solution2(testCases: List[Long], preambleSize: Int): Option[Long] =
    solution1(testCases, preambleSize) flatMap { n =>
      val idx = testCases.indexOf(n)

      (1 until idx).flatMap { sliceSize =>
        (0 until (idx - sliceSize)).flatMap { start =>
          val end = start + sliceSize
          val slice = testCases.slice(start, end)
          (slice.sum == n).option(slice.min + slice.max)
        }
      }.headOption
    }

  def parseLine(line: String): Either[ErrorMessage, Long] =
    line.toLongOption.toRight(ErrorMessage(s"Failed to parse $line"))
