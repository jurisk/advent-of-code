package jurisk.collections.mutable

import jurisk.utils.FromInt
import jurisk.utils.ToInt
import jurisk.utils.conversions.syntax._

import scala.collection.mutable

final class MutableBitSet[T: ToInt](
  private val underlying: mutable.BitSet
) {
  private def this() =
    this(mutable.BitSet.empty)

  def add(value: T): Boolean =
    underlying.add(value.toInt)

  def size: Int =
    underlying.size

  def filterNot(
    predicate: T => Boolean
  )(implicit FI: FromInt[T]): MutableBitSet[T] =
    new MutableBitSet(underlying.filterNot(x => predicate(x.fromInt)))

  def count(
    predicate: T => Boolean
  )(implicit FI: FromInt[T]): Int =
    underlying.count(x => predicate(x.fromInt))

  def contains(value: T): Boolean =
    underlying.contains(value.toInt)
}

object MutableBitSet {
  def empty[T: ToInt]: MutableBitSet[T] =
    new MutableBitSet[T]()

  def apply[T: ToInt](values: T*): MutableBitSet[T] = {
    val result = new MutableBitSet[T]()
    values.foreach(result.add)
    result
  }
}
