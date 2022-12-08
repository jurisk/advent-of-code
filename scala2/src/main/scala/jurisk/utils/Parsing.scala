package jurisk.utils

import jurisk.utils.Geometry.{Coords2D, X, Y}
import jurisk.utils.Utils.IterableOps

import scala.annotation.tailrec

object Parsing {

  /** Splits data (such as a log) into sections, with 'isStart' detecting start
    * of each section
    */
  def splitIntoSections[T](
    data: List[T],
    isStart: T => Boolean,
  ): List[(T, List[T])] = {
    @tailrec
    def f(data: List[T], acc: Vector[(T, List[T])]): Vector[(T, List[T])] =
      data match {
        case Nil    => acc
        case h :: t =>
          val output     = t.takeWhile(x => !isStart(x))
          val thisOutput = (h, output)
          val remains    = t.drop(output.size)
          f(remains, acc :+ thisOutput)
      }

    f(data, Vector.empty).toList
  }

  implicit class StringOps(s: String) {
    def parseCoords2D: Coords2D = {
      val (x, y): (Int, Int) = s.parsePairUnsafe(",", _.trim.toInt)
      Coords2D(X(x), Y(y))
    }

    def splitPairUnsafe(separator: String): (String, String) =
      s.split(separator).toList.twoElementsUnsafe

    def parsePairUnsafe[A, B](
      separator: String,
      parserLeft: String => A,
      parserRight: String => B,
    ): (A, B) = {
      val (a, b) = s.splitPairUnsafe(separator)
      (parserLeft(a), parserRight(b))
    }

    def parsePairUnsafe[T](
      separator: String,
      parser: String => T,
    ): (T, T) =
      s.parsePairUnsafe(separator, parser, parser)

    def parseList[T](
      separator: String,
      parser: String => T,
    ): List[T] =
      s.split(separator).toList.map(parser)
  }
}
