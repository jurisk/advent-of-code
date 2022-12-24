package jurisk.utils

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
}
