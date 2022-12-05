package jurisk.utils

object Utils {
  implicit class IterableOps[T](seq: Iterable[T]) {
    def singleElementUnsafe: T =
      if (seq.size == 1) seq.head
      else
        sys.error(
          s"Expected a single element, but got ${seq.toList.mkString("(", ", ", ")")}"
        )

    def twoElementsUnsafe: (T, T) =
      if (seq.size == 2) (seq.head, seq.tail.head)
      else sys.error(s"Expected two elements, but got $seq")
  }

  implicit class StringOps(s: String) {
    def separatedPairUnsafe(separator: String): (String, String) =
      s.split(separator).toList.twoElementsUnsafe

    def parseSeparatedPairUnsafe[A, B](
      separator: String,
      parserLeft: String => A,
      parserRight: String => B,
    ): (A, B) = {
      val (a, b) = s.separatedPairUnsafe(separator)
      (parserLeft(a), parserRight(b))
    }

    def parseSeparatedPairUnsafe[T](
      separator: String,
      parser: String => T,
    ): (T, T) =
      s.parseSeparatedPairUnsafe(separator, parser, parser)
  }
}
