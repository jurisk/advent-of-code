package jurisk.utils

import jurisk.utils.Geometry.Coords2D
import jurisk.utils.Utils.IterableOps

import javax.swing.JToolBar.Separator

object Parsing {
  implicit class StringOps(s: String) {
    def parseCoords2D: Coords2D = {
      val (x, y): (Int, Int) = s.parsePairUnsafe(",", _.trim.toInt)
      Coords2D.of(x, y)
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
