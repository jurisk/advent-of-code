package jurisk.adventofcode.y2016

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class Advent24Spec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "Advent24" should "work" in {
    Advent24.run
  }
}
