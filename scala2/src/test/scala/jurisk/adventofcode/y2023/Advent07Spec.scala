package jurisk.adventofcode.y2023

import Advent07._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Advent07Spec extends AnyFlatSpec {
  "Camel" should "compare 33332 and 2AAAA" in {
    val a = Hand.parse(PokerGame.Camel, "33332")
    val b = Hand.parse(PokerGame.Camel, "2AAAA")

    val av = Value(PokerGame.Camel, a, None)
    val bv = Value(PokerGame.Camel, b, None)

    println(av)
    println(bv)

    Value.ordering.compare(av, bv) shouldEqual 1 // av > bv
  }

  it should "compare 77888 and 77788" in {
    val a = Hand.parse(PokerGame.Camel, "77888")
    val b = Hand.parse(PokerGame.Camel, "77788")

    val av = Value(PokerGame.Camel, a, None)
    val bv = Value(PokerGame.Camel, b, None)

    println(av)
    println(bv)

    Value.ordering.compare(av, bv) shouldEqual 1 // av > bv
  }

  "Advent07" should "test part 1" in {
    part1(parseFile("2023/07-test.txt")) shouldEqual 6440
  }

  it should "real part 1" in {
    part1(parseFile("2023/07.txt")) shouldEqual 248559379
  }

  it should "test part 2" in {
    part2(parseFile("2023/07-test.txt")) shouldEqual 5905
  }

  it should "real part 2" in {
    part2(parseFile("2023/07.txt")) shouldEqual 0
  }
}
