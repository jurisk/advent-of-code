package jurisk.collections

import jurisk.utils.Parsing.StringOps

class BiMap[A, B] private (
  private val leftToRightUnderlying: Map[A, B],
  private val rightToLeftUnderlying: Map[B, A],
) {
  def leftToRight(left: A): Option[B]  = leftToRightUnderlying.get(left)
  def rightToLeft(right: B): Option[A] = rightToLeftUnderlying.get(right)

  def leftToRightUnsafe(left: A): B  =
    leftToRight(left).getOrElse(s"Failed to find $left".fail)
  def rightToLeftUnsafe(right: B): A =
    rightToLeft(right).getOrElse(s"Failed to find $right".fail)

  def leftKeys: Iterable[A] = leftToRightUnderlying.keys
  def rightKeys: Iterable[B] = rightToLeftUnderlying.keys
}

object BiMap {
  implicit final class BiDirectionalArrowAssociation[A](private val self: A)
      extends AnyVal {
    @inline def <->[B](y: B): (A, B) = (self, y)
  }

  def from[A, B](it: IterableOnce[(A, B)]): BiMap[A, B] = {
    val leftToRight: Map[A, B] = Map.from(it)
    val rightToLeft: Map[B, A] = Map.from(leftToRight.toList.map(_.swap))
    require(leftToRight.size == rightToLeft.size)

    new BiMap[A, B](
      leftToRight,
      rightToLeft,
    )
  }

  def apply[A, B](elems: (A, B)*): BiMap[A, B] = from(elems)
}
