package jurisk.adventofcode

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec

abstract class AdventAppSpec[TestCase, Output1, Output2](app: AdventApp[TestCase, Output1, Output2]) extends AsyncFreeSpec with AsyncIOSpec {
  protected def loadTestData(suffix: String): List[TestCase] = app.parseTestData(suffix).unsafeRunSync().getOrElse(sys.error("failed"))
  protected lazy val testData00: List[TestCase] = loadTestData("00")
  protected val realData: List[TestCase] = app.parseRealData.unsafeRunSync().getOrElse(sys.error("failed"))
}
