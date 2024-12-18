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

  def contains(value: T): Boolean =
    underlying.contains(value.toInt)

  def +(value: T): ImmutableBitSet[T] =
    new ImmutableBitSet(underlying + value.toInt)

  def ++(other: ImmutableBitSet[T]): ImmutableBitSet[T] =
    new ImmutableBitSet(underlying ++ other.underlying)
}

object ImmutableBitSet {
  def fromSpecific[T: ToInt](iterable: IterableOnce[T]): ImmutableBitSet[T] =
    new ImmutableBitSet[T](BitSet.fromSpecific(iterable.iterator.map(_.toInt)))

  def empty[T: ToInt]: ImmutableBitSet[T] = new ImmutableBitSet[T]()
}
