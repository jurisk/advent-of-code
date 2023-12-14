package jurisk.utils

import jurisk.collections.LRUCache

import scala.collection.mutable

object Memoize {
  // Call only once, not for every invocation
  def memoizeSynchronized[I, O](f: I => O): I => O = {
    val cache = mutable.Map[I, O]()
    (key: I) => cache.synchronized(cache.getOrElseUpdate(key, f(key)))
  }

  // Call only once, not for every invocation
  def memoize[I, O](f: I => O): I => O = {
    val cache = mutable.Map[I, O]()
    (key: I) => cache.getOrElseUpdate(key, f(key))
  }

  // Call only once, not for every invocation
  def memoizeWithCapacity[I, O](f: I => O, capacity: Int): I => O = {
    val cache = LRUCache[I, O](capacity)
    (key: I) => cache.getOrElseUpdate(key, f(key))
  }

  // Call only once, not for every invocation
  def memoizeWithCapacitySynchronized[I, O](
    f: I => O,
    capacity: Int,
  ): I => O = {
    val cache = LRUCache[I, O](capacity)
    (key: I) => cache.synchronized(cache.getOrElseUpdate(key, f(key)))
  }
}
