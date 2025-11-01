package jurisk.adventofcode

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers.shouldEqual

abstract class AdventAppSpec[Input, Output1, Output2](
  app: AdventApp[Input, Output1, Output2]
) extends AsyncFreeSpec
    with AsyncIOSpec {
  protected def loadTestData(suffix: String): Input =
    app.parseTestData(suffix).unsafeRunSync().getOrElse(sys.error("failed"))
  protected lazy val testData00: Input              = loadTestData("00")
  protected lazy val testData01: Input              = loadTestData("01")
  protected val realData: Input                     =
    app.parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))

  protected def runTest(
    name: String,
    testData: Input,
    expected1: Output1,
    expected2: Output2,
  ): Unit =
    name - {
      "solution1" in {
        app.solution1(testData) shouldEqual expected1
      }

      "solution2" in {
        app.solution2(testData) shouldEqual expected2
      }
    }
}
