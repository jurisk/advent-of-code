package jurisk.utils

object Utils {
  implicit class IterableOps[T](seq: Iterable[T]) {
    def counts: Map[T, Int] = seq.groupMapReduce(identity)(_ => 1)(_ + _)

    def allDistinct: Boolean = seq.toSet.size == seq.size

    def singleElementUnsafe: T =
      if (seq.size == 1) seq.head
      else
        sys.error(
          s"Expected a single element, but got ${seq.toList.mkString("(", ", ", ")")}"
        )

    // We just keep mistyping this so much, we may as well add it :shrug:
    def singleResultUnsafe: T = singleElementUnsafe

    def twoElementsUnsafe: (T, T) =
      if (seq.size == 2) (seq.head, seq.tail.head)
      else sys.error(s"Expected two elements, but got $seq")
  }
}
