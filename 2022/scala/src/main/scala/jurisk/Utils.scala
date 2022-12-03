package jurisk

object Utils {
  implicit class IterableOps[T](seq: Iterable[T]) {
    def singleElementUnsafe: T =
      if (seq.size == 1) seq.head
      else sys.error(s"Expected a single element, but got $seq")
  }
}
