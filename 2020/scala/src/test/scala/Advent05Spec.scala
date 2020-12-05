import Advent05._

import cats.implicits._

object Advent05Spec extends App:
  def check(input: String, expected: Int) = {
    val obtained = seatId(input)
    assert(obtained == expected.asRight, s"Failed $input $expected - got $obtained")
  }

  check("FBFBBFFRLR", 357)
  check("BFFFBBFRRR", 567)
  check("FFFBBBFRRR", 119)
  check("BBFFBBFRLL", 820)

  println("Passed")

