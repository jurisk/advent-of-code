package jurisk.collections.mutable

import jurisk.collections.immutable.BiMap

// https://en.wikipedia.org/wiki/Disjoint-set_data_structure
trait DisjointSets[T] {
  def union(a: T, b: T): Unit
  // Which set does `value` belong to?
  def find(value: T): T
  def toSets: Set[Set[T]]
}

private class DisjointSetsImpl[T](labels: T*) extends DisjointSets[T] {
  // https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Making_new_sets
  private type Index = Int
  private val parents: Array[Index]    = labels.indices.toArray
  private val sizes: Array[Int]        = Array.fill(labels.size)(1)
  private val mapping: BiMap[T, Index] = BiMap.from(labels.zipWithIndex)

  override def union(a: T, b: T): Unit =
    union(mapping.leftToRightUnsafe(a), mapping.leftToRightUnsafe(b))

  // https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Union_by_size
  private def union(a: Index, b: Index): Unit = {
    // Replace nodes by roots
    val x = find(a)
    val y = find(b)

    // a and b are not in the same set
    if (x != y) {
      // If necessary, swap variables to ensure that
      // x has at least as many descendants as y
      val (a, b) = if (sizes(x) < sizes(y)) {
        (y, x)
      } else {
        (x, y)
      }

      // Make a the new root
      parents(b) = a
      // Update the size of a
      sizes(a) = sizes(a) + sizes(b)
    }
  }

  // https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Finding_set_representatives
  // This is not the most efficient `find`, but it is the simplest.
  private def find(value: Index): Index = {
    var x    = value
    var root = x
    while (parents(root) != root)
      root = parents(root)

    while (parents(x) != root) {
      val parent = parents(root)
      parents(x) = root
      x = parent
    }

    root
  }

  override def find(value: T): T = {
    val found = find(mapping.leftToRightUnsafe(value))
    mapping.rightToLeftUnsafe(found)
  }

  override def toSets: Set[Set[T]] =
    parents.indices
      .groupBy(find)
      .values
      .map(results => results.map(mapping.rightToLeftUnsafe).toSet)
      .toSet
}

object DisjointSets {
  def apply[T](labels: T*): DisjointSets[T] =
    new DisjointSetsImpl[T](labels: _*)

  final case class Entry[T](
    parent: T,
    size: Int,
  )
}
