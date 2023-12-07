package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent07 {
  type Input = List[HandWithBid]

  sealed trait PokerGame

  object PokerGame {
    final case object Camel1 extends PokerGame
    final case object Camel2 extends PokerGame
  }

  final case class Hand(ranks: List[Rank])

  object Hand {
    def parse(x: String): Hand =
      Hand((x map Rank.parse).toList)
  }

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

    data
      .sortBy(_.hand)
      .zipWithIndex
      .map { case (x, idx) =>
        x.bid * (idx + 1)
      }
      .sum
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
