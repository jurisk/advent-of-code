package jurisk.adventofcode

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec

abstract class AdventAppSpec[Input, Output1, Output2](app: AdventApp[Input, Output1, Output2]) extends AsyncFreeSpec with AsyncIOSpec {
  protected def loadTestData(suffix: String): Input = app.parseTestData(suffix).unsafeRunSync().getOrElse(sys.error("failed"))
  protected lazy val testData00: Input = loadTestData("00")
  protected lazy val testData01: Input = loadTestData("01")
  protected val realData: Input = app.parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))
}
