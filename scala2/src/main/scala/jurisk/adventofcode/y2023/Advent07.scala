package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

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
            Hand.parse(PokerGame.Camel, hand),
            bid.toInt,
          )
        case _             => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parseList("\n", HandWithBid.parse)

  def solve(
    pokerGame: PokerGame,
    board: Option[Board],
    hands: Set[Hand],
  ): List[Set[Hand]] = {
    implicit val ordering: Ordering[Hand] =
      Value.orderingForBoard(pokerGame, board)
    Sort.consideringEqualValues(hands)
  }

  def part1(data: Input): Int = {
    val results: List[Hand] =
      solve(PokerGame.Camel, None, data.toSet.map((x: HandWithBid) => x.hand))
        .map(x => x.toList.head)

    results.zipWithIndex.map { case (x, idx) =>
      val found = data.find(_.hand == x).get
      found.bid * (idx + 1)
    }.sum
  }

  def part2(data: Input): Int =
    data.length + 1234567

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/07.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
