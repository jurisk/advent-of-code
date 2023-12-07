package jurisk.math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DiscreteIntervalSetSpec extends AnyFlatSpec {
  "DiscreteIntervalSet" should "union with empty" in {
    val empty = DiscreteIntervalSet.empty[Int]
    val some  = DiscreteIntervalSet.fromInterval(4, 6)

    (empty union some) shouldEqual some
    (some union empty) shouldEqual some
  }

  it should "work as union" in {
    val a = DiscreteIntervalSet.fromInterval(4, 6)
    val b = DiscreteIntervalSet.fromInterval(8, 11)
    val c = DiscreteIntervalSet(
      DiscreteInterval(4, 6),
      DiscreteInterval(8, 11),
    )
    (a union b) shouldEqual c
    (b union a) shouldEqual c
  }

  it should "be able to insert interval in the gap" in {
    val set    = DiscreteIntervalSet.fromInterval(
      2,
      5,
    ) add DiscreteInterval(9, 11)
    val result = set add DiscreteInterval(6, 8)
    result shouldEqual DiscreteIntervalSet(
      DiscreteInterval(2, 11)
    )
  }
}
