package jurisk

package object utils {
  // Call only once, not for every invocation
  def memoizeSynchronized[I, O](f: I => O): I => O =
    new scala.collection.mutable.HashMap[I, O]() {
      self =>
      override def apply(key: I): O = self.synchronized(getOrElseUpdate(key, f(key)))
    }

  // Call only once, not for every invocation
  def memoize[I, O](f: I => O): I => O =
    new scala.collection.mutable.HashMap[I, O]() {
      self =>
      override def apply(key: I): O = getOrElseUpdate(key, f(key))
    }
}
