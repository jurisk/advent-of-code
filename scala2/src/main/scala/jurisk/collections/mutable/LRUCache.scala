package jurisk.collections.mutable

import java.util.{LinkedHashMap => JavaLinkedHashMap}
import java.util.{Map => JavaMap}

private class LRUCache[K, V](capacity: Int) extends MapForCache[K, V] {
  // We tried using Scala mutable.LinkedHashMap here but it didn't have such a method to override, so it was slightly
  // more code
  private val cache: JavaLinkedHashMap[K, V] = new JavaLinkedHashMap[K, V]() {
    override def removeEldestEntry(eldest: JavaMap.Entry[K, V]): Boolean =
      size > capacity
  }

  def getOrElseUpdate(key: K, op: => V): V =
    Option(cache.remove(key)) match {
      case Some(value) =>
        // The key exists, move it to the end to mark it as recently used, and return it
        cache.put(key, value)
        value

      case None =>
        // The key doesn't exist, compute the value, update the cache, and return it
        val newValue = op
        cache.put(key, newValue)
        newValue
    }

  def get(key: K): Option[V] =
    Option(cache.remove(key)).map { value =>
      cache.put(key, value)
      value
    }

  def put(key: K, value: V): Option[V] = {
    val previous = Option(cache.remove(key))
    cache.put(key, value)
    previous
  }
}

object LRUCache {
  def apply[K, V](capacity: Int): MapForCache[K, V] =
    new LRUCache[K, V](capacity)
}
