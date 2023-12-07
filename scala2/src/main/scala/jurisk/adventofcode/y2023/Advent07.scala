package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps

object Advent07 {
  type Input = List[HandWithBid]

  final case class HandWithBid(
    hand: Hand,
    bid: Int,
  )

  object HandWithBid {
    def parse(s: String): HandWithBid =
      s match {
        case s"$hand $bid" =>
          HandWithBid(
            Hand.parse(hand),
            bid.toInt,
          )
        case _             => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parseList("\n", HandWithBid.parse)

  def solve(data: Input, game: PokerGame): Int = {
    implicit val ordering: Ordering[Hand] =
      Value.orderingForGame(game)

    val results: List[Hand] = {
      val hands = data.toSet.map((x: HandWithBid) => x.hand)
      Sort
        .consideringEqualValues(hands)
        .map(x => x.singleElementUnsafe)
    }

    results.zipWithIndex.map { case (x, idx) =>
      val found = data.find(_.hand == x).get
      found.bid * (idx + 1)
    }.sum
  }

  def part1(data: Input): Int = solve(data, PokerGame.Camel1)
  def part2(data: Input): Int = solve(data, PokerGame.Camel2)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/07.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
