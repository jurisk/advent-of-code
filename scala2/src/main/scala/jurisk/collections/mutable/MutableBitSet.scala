package jurisk.collections.mutable

import jurisk.collections.mutable.BitSetKeySyntax.BitSetKeyOps1
import jurisk.collections.mutable.BitSetKeySyntax.BitSetKeyOps2

import scala.collection.mutable

trait BitSetKey[T] {
  def toInt(value: T): Int
  def fromInt(value: Int): T
}

object BitSetKeyInstances {
  implicit val intBitSetKey: BitSetKey[Int] = new BitSetKey[Int] {
    def toInt(value: Int): Int   = value
    def fromInt(value: Int): Int = value
  }
}

object BitSetKeySyntax {
  implicit class BitSetKeyOps1[T](value: T)(implicit ev: BitSetKey[T]) {
    def toInt: Int = ev.toInt(value)
  }

  implicit class BitSetKeyOps2[T](value: Int)(implicit ev: BitSetKey[T]) {
    def fromInt: T = ev.fromInt(value)
  }
}

final class MutableBitSet[T: BitSetKey](
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
  ): MutableBitSet[T] =
    new MutableBitSet(underlying.filterNot(x => predicate(x.fromInt)))

  def count(
    predicate: T => Boolean
  ): Int =
    underlying.count(x => predicate(x.fromInt))
}

object MutableBitSet {
  def apply[T: BitSetKey](values: T*): MutableBitSet[T] = {
    val result = new MutableBitSet[T]()
    values.foreach(result.add)
    result
  }
}
