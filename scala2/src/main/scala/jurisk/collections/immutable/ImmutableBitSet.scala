package jurisk.collections.immutable

import jurisk.utils.ToInt
import jurisk.utils.conversions.syntax.ToIntOps

import scala.collection.immutable.BitSet

final class ImmutableBitSet[T: ToInt](
  private val underlying: BitSet = BitSet.empty
) {
  private def this() =
    this(BitSet.empty)

  def size: Int =
    underlying.size

  def +(value: T): ImmutableBitSet[T] =
    new ImmutableBitSet(underlying + value.toInt)

  def ++(other: ImmutableBitSet[T]): ImmutableBitSet[T] =
    new ImmutableBitSet(underlying ++ other.underlying)
}

object ImmutableBitSet {
  def empty[T: ToInt]: ImmutableBitSet[T] = new ImmutableBitSet[T]()
}
