package jurisk.utils

object Utils {
  implicit class IterableOps[T](seq: Iterable[T]) {
    def counts: Map[T, Int] = seq.groupMapReduce(identity)(_ => 1)(_ + _)

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
}
