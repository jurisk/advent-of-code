package jurisk.utils

import cats.implicits._
import jurisk.geometry.Coords2D
import jurisk.utils.CollectionOps.IterableOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

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
    def fail: Nothing = sys.error(s)

    def failedToParse: Nothing = s"Failed to parse `$s`".fail

    def removePrefix(prefix: String): Option[String] =
      if (s.startsWith(prefix)) {
        s.drop(prefix.length).some
      } else {
        none
      }

    def removePrefixUnsafe(prefix: String): String =
      removePrefix(prefix).getOrElse(
        sys.error(s"Expected '$s' to start with '$prefix' but it did not")
      )

    def parseCoords2D: Coords2D = {
      val (x, y): (Int, Int) = s.parsePairUnsafe(",", _.trim.toInt)
      Coords2D(x, y)
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

    def parseLines[T](
      parser: String => T
    ): List[T] = parseList("\n", parser)

    private def extractDigitStrings: Regex.MatchIterator = {
      val RegEx = """([-+]?\d+)""".r
      RegEx.findAllIn(s)
    }

    def extractInts: Iterator[Int]   = extractDigitStrings.map(_.toInt)
    def extractLongs: Iterator[Long] = extractDigitStrings.map(_.toLong)

    def extractIntList: List[Int]   = extractInts.toList
    def extractLongList: List[Long] = extractLongs.toList

    def extractIntVector: Vector[Int]   = extractInts.toVector
    def extractLongVector: Vector[Long] = extractLongs.toVector

    def extractIntArraySeq: ArraySeq[Int]   = ArraySeq.from(extractInts)
    def extractLongArraySeq: ArraySeq[Long] = ArraySeq.from(extractLongs)
  }
}
