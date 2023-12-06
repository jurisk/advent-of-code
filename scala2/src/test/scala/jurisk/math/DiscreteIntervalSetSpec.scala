package jurisk.math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DiscreteIntervalSetSpec extends AnyFlatSpec {
  "DiscreteIntervalSet" should "union with empty" in {
    val empty = DiscreteIntervalSet.empty[Int]
    val some  = DiscreteIntervalSet.fromInclusiveInterval(4, 6)

    (empty union some) shouldEqual some
    (some union empty) shouldEqual some
  }

  it should "work as union" in {
    val a = DiscreteIntervalSet.fromInclusiveInterval(4, 6)
    val b = DiscreteIntervalSet.fromInclusiveInterval(8, 11)
    val c = DiscreteIntervalSet(
      Set(DiscreteInterval(4, 6), DiscreteInterval(8, 11))
    )
    (a union b) shouldEqual c
  }
}
