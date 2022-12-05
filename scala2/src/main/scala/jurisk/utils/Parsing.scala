package jurisk.utils

import jurisk.utils.Utils.IterableOps

object Parsing {
  implicit class StringOps(s: String) {
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
  }
}
