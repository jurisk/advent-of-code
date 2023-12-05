package jurisk.math

import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

class InclusiveDiscreteIntervalSpec extends AnyFlatSpec {
  // https://i.stack.imgur.com/h2Nw2.png
  "InclusiveDiscreteInterval" should "subtract after" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      36,
      41,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(11, 30))
    )
  }

  it should "subtract frontoverlap" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      5,
      20,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(21, 30))
    )
  }

  it should "subtract backoverlap" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      30,
      33,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(11, 29))
    )
  }

  it should "subtract enclosing" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      8,
      35,
    ) shouldEqual NonOverlappingDiscreteIntervalSet.empty[Int]
  }

  it should "subtract before" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      3,
      8,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(11, 30))
    )
  }

  it should "subtract enclosed" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      18,
      27,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(
        InclusiveDiscreteInterval(11, 17),
        InclusiveDiscreteInterval(28, 30),
      )
    )
  }

  it should "add after" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      36,
      41,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(
        InclusiveDiscreteInterval(11, 30),
        InclusiveDiscreteInterval(36, 41),
      )
    )
  }

  it should "add frontoverlap" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      5,
      20,
    ) shouldEqual
      NonOverlappingDiscreteIntervalSet(Set(InclusiveDiscreteInterval(5, 30)))
  }

  it should "add backoverlap" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      30,
      33,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(11, 33))
    )
  }

  it should "add enclosing" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      8,
      35,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(8, 35))
    )
  }

  it should "add before" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      3,
      8,
    ) shouldEqual
      NonOverlappingDiscreteIntervalSet(
        Set(
          InclusiveDiscreteInterval(3, 8),
          InclusiveDiscreteInterval(11, 30),
        )
      )
  }

  it should "add enclosed" in {
    InclusiveDiscreteInterval(11, 30) union InclusiveDiscreteInterval(
      18,
      27,
    ) shouldEqual
      NonOverlappingDiscreteIntervalSet(
        Set(
          InclusiveDiscreteInterval(11, 30)
        )
      )
  }

  it should "union if touching" in {
    InclusiveDiscreteInterval(5, 8) union InclusiveDiscreteInterval(
      2,
      4,
    ) shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(2, 8))
    )
  }

  it should "be able to insert interval in the gap" ignore {
    val set    = NonOverlappingDiscreteIntervalSet.createInclusive(
      2,
      5,
    ) add InclusiveDiscreteInterval(9, 11)
    val result = set add InclusiveDiscreteInterval(6, 8)
    result shouldEqual NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(2, 11))
    )
  }

  "NonOverlappingDiscreteIntervalSet" should "union with empty" in {
    val empty = NonOverlappingDiscreteIntervalSet.empty[Int]
    val some  = NonOverlappingDiscreteIntervalSet.createInclusive(4, 6)

    (empty union some) shouldEqual some
    (some union empty) shouldEqual some
  }

  it should "work as union" in {
    val a = NonOverlappingDiscreteIntervalSet.createInclusive(4, 6)
    val b = NonOverlappingDiscreteIntervalSet.createInclusive(8, 11)
    val c = NonOverlappingDiscreteIntervalSet(
      Set(InclusiveDiscreteInterval(4, 6), InclusiveDiscreteInterval(8, 11))
    )
    (a union b) shouldEqual c
  }
}
