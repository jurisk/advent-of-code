package jurisk.collections.mutable

trait MapForCache[K, V] {
  def getOrElseUpdate(key: K, op: => V): V
}
