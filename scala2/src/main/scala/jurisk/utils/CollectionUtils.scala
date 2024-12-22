package jurisk.utils

object CollectionUtils {
  def mergeMaps[K, V](maps: Seq[Map[K, V]]): Map[K, Seq[V]] =
    maps
      .flatMap(_.toList)
      .groupBy { case (k, _) => k }
      .map { case (k, v1) => k -> v1.map { case (_, v2) => v2 } }
}
