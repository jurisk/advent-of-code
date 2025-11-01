package jurisk.adventofcode.y2023

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers._

import Advent05._

class Advent05Spec extends AsyncFlatSpec with AsyncIOSpec {
  "Advent05" should "test part 1" in
    part1(parseFile("2023/05-test.txt")).asserting(_ shouldEqual 35L)

  it should "real part 1" in
    part1(parseFile("2023/05.txt")).asserting(_ shouldEqual 424490994L)

  it should "test part 2" in
    part2(parseFile("2023/05-test.txt")).asserting(_ shouldEqual 46L)

  it should "real part 2" in
    part2(parseFile("2023/05.txt")).asserting(_ shouldEqual 15290096L)
}
