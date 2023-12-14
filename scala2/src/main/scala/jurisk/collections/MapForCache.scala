package jurisk.collections

trait MapForCache[K, V] {
  def getOrElseUpdate(key: K, op: => V): V
}
