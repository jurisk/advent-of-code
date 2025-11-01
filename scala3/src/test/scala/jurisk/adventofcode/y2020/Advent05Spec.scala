package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.y2020.Advent05._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent05Spec extends AnyFreeSpec:
  "seatId" - {
    "FBFBBFFRLR" in {
      seatId("FBFBBFFRLR") shouldEqual 357.asRight
    }
    "BFFFBBFRRR" in {
      seatId("BFFFBBFRRR") shouldEqual 567.asRight
    }
    "FFFBBBFRRR" in {
      seatId("FFFBBBFRRR") shouldEqual 119.asRight
    }
    "BBFFBBFRLL" in {
      seatId("BBFFBBFRLL") shouldEqual 820.asRight
    }
  }
