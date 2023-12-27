package jurisk.utils

import jurisk.collections.mutable.{LRUCache, MapForCache}

import scala.collection.mutable

// Call only once, not for every invocation
object Memoize {
  private def createMap[K, V](
    capacity: Option[Int] = None,
    synchronized: Boolean = true,
  ): MapForCache[K, V] = {
    val result = capacity match {
      case Some(capacity) => LRUCache[K, V](capacity)
      case None           =>
        val underlying = mutable.Map[K, V]()
        new MapForCache[K, V] {
          def getOrElseUpdate(key: K, op: => V): V =
            underlying.getOrElseUpdate(key, op)
        }
    }

    if (synchronized) {
      new MapForCache[K, V] {
        override def getOrElseUpdate(key: K, op: => V): V =
          result.synchronized(result.getOrElseUpdate(key, op))
      }
    } else {
      result
    }
  }

  def memoize1[I, O](
    f: I => O,
    capacity: Option[Int] = None,
    synchronized: Boolean = true,
  ): I => O = {
    val cache = createMap[I, O](capacity, synchronized)
    (key: I) => cache.getOrElseUpdate(key, f(key))
  }

  def memoize2[A, B, O](
    f: (A, B) => O,
    capacity: Option[Int] = None,
    synchronized: Boolean = true,
  ): (A, B) => O = {
    val cache = createMap[(A, B), O](capacity, synchronized)
    (a: A, b: B) => cache.getOrElseUpdate((a, b), f(a, b))
  }

  def memoize3[A, B, C, O](
    f: (A, B, C) => O,
    capacity: Option[Int] = None,
    synchronized: Boolean = true,
  ): (A, B, C) => O = {
    val cache = createMap[(A, B, C), O](capacity, synchronized)
    (a: A, b: B, c: C) => cache.getOrElseUpdate((a, b, c), f(a, b, c))
  }
}
