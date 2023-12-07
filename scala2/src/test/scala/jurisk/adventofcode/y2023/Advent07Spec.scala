package jurisk.adventofcode.y2023

import Advent07._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Advent07Spec extends AnyFlatSpec {
  private def parseValue(game: PokerGame, ranks: String): Value =
    game.handValue(Hand.parse(ranks))

  "Camel 1" should "compare 33332 and 2AAAA" in {
    val a = parseValue(PokerGame.Camel1, "33332")
    val b = parseValue(PokerGame.Camel1, "2AAAA")

    a should be > b
  }

  it should "compare 77888 and 77788" in {
    val a = parseValue(PokerGame.Camel1, "77888")
    val b = parseValue(PokerGame.Camel1, "77788")

    a should be > b
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
    part2(parseFile("2023/07.txt")) shouldEqual 249631254
  }
}
