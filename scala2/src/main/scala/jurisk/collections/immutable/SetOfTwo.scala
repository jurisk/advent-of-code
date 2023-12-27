package jurisk.collections.immutable

final case class SetOfTwo[T](private val underlying: Set[T]) {
  def tupleInArbitraryOrder: (T, T) = {
    val List(a, b) = underlying.toList
    (a, b)
  }

  def contains(elem: T): Boolean = underlying.contains(elem)

  def toSet: Set[T] = underlying

  override def toString: String = tupleInArbitraryOrder.toString()

  def mapUnsafe[B](f: T => B): SetOfTwo[B] = SetOfTwo(toSet.map(f))
  def map[B](f: T => B): Set[B]            = toSet.map(f)
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
