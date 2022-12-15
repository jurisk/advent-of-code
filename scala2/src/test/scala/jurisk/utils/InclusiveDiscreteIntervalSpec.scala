package jurisk.utils

import org.scalatest.matchers.should.Matchers._
import org.scalatest.flatspec.AnyFlatSpec

class InclusiveDiscreteIntervalSpec extends AnyFlatSpec {
  // https://i.stack.imgur.com/h2Nw2.png
  "InclusiveDiscreteInterval" should "a" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      36,
      41,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 30))
  }

  it should "b" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      5,
      20,
    ) shouldEqual Set(InclusiveDiscreteInterval(21, 30))
  }

  it should "c" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      30,
      33,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 29))
  }

  it should "d" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      8,
      35,
    ) shouldEqual Set.empty
  }

  it should "e" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      3,
      8,
    ) shouldEqual Set(InclusiveDiscreteInterval(11, 30))
  }

  it should "f" in {
    InclusiveDiscreteInterval(11, 30) subtract InclusiveDiscreteInterval(
      18,
      27,
    ) shouldEqual Set(
      InclusiveDiscreteInterval(11, 17),
      InclusiveDiscreteInterval(28, 30),
    )
  }
}
