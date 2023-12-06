package jurisk.math

import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

class DiscreteIntervalSpec extends AnyFlatSpec {
  // https://i.stack.imgur.com/h2Nw2.png
  "InclusiveDiscreteInterval" should "subtract after" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      36,
      41,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(11, 30))
    )
  }

  it should "subtract frontoverlap" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      5,
      20,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(21, 30))
    )
  }

  it should "subtract backoverlap" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      30,
      33,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(11, 29))
    )
  }

  it should "subtract enclosing" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      8,
      35,
    ) shouldEqual DiscreteIntervalSet.empty[Int]
  }

  it should "subtract before" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      3,
      8,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(11, 30))
    )
  }

  it should "subtract enclosed" in {
    DiscreteInterval(11, 30) subtract DiscreteInterval(
      18,
      27,
    ) shouldEqual DiscreteIntervalSet(
      Set(
        DiscreteInterval(11, 17),
        DiscreteInterval(28, 30),
      )
    )
  }

  it should "add after" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      36,
      41,
    ) shouldEqual DiscreteIntervalSet(
      Set(
        DiscreteInterval(11, 30),
        DiscreteInterval(36, 41),
      )
    )
  }

  it should "add frontoverlap" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      5,
      20,
    ) shouldEqual
      DiscreteIntervalSet(Set(DiscreteInterval(5, 30)))
  }

  it should "add backoverlap" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      30,
      33,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(11, 33))
    )
  }

  it should "add enclosing" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      8,
      35,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(8, 35))
    )
  }

  it should "add before" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      3,
      8,
    ) shouldEqual
      DiscreteIntervalSet(
        Set(
          DiscreteInterval(3, 8),
          DiscreteInterval(11, 30),
        )
      )
  }

  it should "add enclosed" in {
    DiscreteInterval(11, 30) union DiscreteInterval(
      18,
      27,
    ) shouldEqual
      DiscreteIntervalSet(
        Set(
          DiscreteInterval(11, 30)
        )
      )
  }

  it should "union if touching" in {
    DiscreteInterval(5, 8) union DiscreteInterval(
      2,
      4,
    ) shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(2, 8))
    )
  }

  it should "be able to insert interval in the gap" ignore {
    val set    = DiscreteIntervalSet.fromInclusiveInterval(
      2,
      5,
    ) add DiscreteInterval(9, 11)
    val result = set add DiscreteInterval(6, 8)
    result shouldEqual DiscreteIntervalSet(
      Set(DiscreteInterval(2, 11))
    )
  }

  "NonOverlappingDiscreteIntervalSet" should "union with empty" in {
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
