package jurisk.collections.mutable

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DisjointSetsSpec extends AnyFreeSpec {
  val a = "a"
  val b = "b"
  val c = "c"
  val d = "d"
  val e = "e"

  "work" in {
    val ds = DisjointSets(a, b, c, d, e)
    ds.toSets shouldEqual Set(Set(a), Set(b), Set(c), Set(d), Set(e))
    ds.union(a, b)
    ds.toSets shouldEqual Set(Set(a, b), Set(c), Set(d), Set(e))
    ds.union(a, b)
    ds.toSets shouldEqual Set(Set(a, b), Set(c), Set(d), Set(e))
    ds.union(d, e)
    ds.toSets shouldEqual Set(Set(a, b), Set(c), Set(d, e))
    ds.union(a, e)
    ds.toSets shouldEqual Set(Set(a, b, d, e), Set(c))
    ds.union(d, c)
    ds.toSets shouldEqual Set(Set(a, b, c, d, e))
  }
}
