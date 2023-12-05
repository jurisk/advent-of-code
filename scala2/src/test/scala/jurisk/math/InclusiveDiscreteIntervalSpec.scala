package jurisk.math

import jurisk.math.InclusiveDiscreteInterval
import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

class InclusiveDiscreteIntervalSpec extends AnyFlatSpec {
  // https://i.stack.imgur.com/h2Nw2.png
  "InclusiveDiscreteInterval" should "subtract after" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      36,
      41,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 30))
  }

  it should "subtract frontoverlap" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      5,
      20,
    ) shouldEqual Set(InclusiveDiscreteInterval(21, 30))
  }

  it should "subtract backoverlap" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      30,
      33,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 29))
  }

  it should "subtract enclosing" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      8,
      35,
    ) shouldEqual Set.empty
  }

  it should "subtract before" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      3,
      8,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 30))
  }

  it should "subtract enclosed" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      18,
      27,
    ) shouldEqual Set(
      InclusiveDiscreteInterval(11, 17),
      InclusiveDiscreteInterval(28, 30),
    )
  }

  it should "add after" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      36,
      41,
    ) shouldEqual Set(
      InclusiveDiscreteInterval(11, 30),
      InclusiveDiscreteInterval(36, 41),
    )
  }

  it should "add frontoverlap" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      5,
      20,
    ) shouldEqual
      Set(InclusiveDiscreteInterval(5, 30))
  }

  it should "add backoverlap" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      30,
      33,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 33))
  }

  it should "add enclosing" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      8,
      35,
    ) shouldEqual Set(InclusiveDiscreteInterval(8, 35))
  }

  it should "add before" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      3,
      8,
    ) shouldEqual
      Set(
        InclusiveDiscreteInterval(3, 8),
        InclusiveDiscreteInterval(11, 30),
      )
  }

  it should "add enclosed" in {
    InclusiveDiscreteInterval(11, 30) add InclusiveDiscreteInterval(
      18,
      27,
    ) shouldEqual
      Set(
        InclusiveDiscreteInterval(11, 30)
      )
  }

  // TODO: Fix this
  it should "add touching" ignore {
    InclusiveDiscreteInterval(5, 8) add InclusiveDiscreteInterval(
      2,
      4,
    ) shouldEqual Set(InclusiveDiscreteInterval(2, 8))
  }

  // TODO: Fix this
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
}
