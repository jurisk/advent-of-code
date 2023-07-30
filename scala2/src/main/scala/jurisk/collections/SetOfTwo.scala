package jurisk.collections

final case class SetOfTwo[T](private val underlying: Set[T]) {
  def tupleInArbitraryOrder: (T, T) = {
    val List(a, b) = underlying.toList
    (a, b)
  }

  override def toString: String = tupleInArbitraryOrder.toString()
}

object SetOfTwo {
  def apply[T](a: T, b: T): SetOfTwo[T] = {
    val underlying = Set(a, b)
    assert(
      underlying.size == 2,
      s"$underlying should have exactly two elements",
    )
    SetOfTwo(underlying)
  }

  def fromList[T](list: List[T]): SetOfTwo[T] = {
    val List(a, b) = list
    SetOfTwo(a, b)
  }
}
