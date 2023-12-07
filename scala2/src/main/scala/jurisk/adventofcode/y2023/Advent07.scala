package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import cats.implicits._
import scala.annotation.tailrec

object Advent07 {
  type Input = List[HandWithBid]

  sealed trait PokerGame

  object PokerGame {
    final case object Camel1 extends PokerGame
    final case object Camel2 extends PokerGame
  }

  final case class Rank(value: Char) {
    def strength: Int = Rank.Ordered.indexOf(this)
  }

  object Rank {
    val Ordered: List[Rank]          = "*23456789TJQKA".toList.map(Rank(_))
    val Wildcard: Rank               = Rank('*')
    val NonWildCardRanks: List[Rank] =
      Rank.Ordered.filterNot(_ == Rank.Wildcard)

    def parse(x: Char): Rank =
      Ordered
        .find(_.value === x)
        .getOrElse(sys.error(s"Failed to parse Rank $x"))
  }

  final case class Hand(ranks: List[Rank])

  object Hand {
    def parse(x: String): Hand =
      Hand((x map Rank.parse).toList)
  }

  final case class Value(kind: ValueKind, originalRankList: List[Rank])

  object Value {
    implicit private val rankOrdering: Ordering[Rank] = (x: Rank, y: Rank) =>
      Ordering[Int].compare(x.strength, y.strength)

    @tailrec
    private def compareRankLists(x: List[Rank], y: List[Rank]): Int = {
      require(x.length === y.length)
      if (x.isEmpty) 0
      else {
        val result = Ordering[Rank].compare(x.head, y.head)
        if (result != 0) result else compareRankLists(x.tail, y.tail)
      }
    }

    implicit private def rankListOrdering: Ordering[List[Rank]] =
      (x: List[Rank], y: List[Rank]) => compareRankLists(x, y)

    def orderingForGame(
      pokerGame: PokerGame
    ): Ordering[Hand] = (x: Hand, y: Hand) =>
      ordering.compare(
        Value(pokerGame, x),
        Value(pokerGame, y),
      )

    private def determineKind(cards: List[Rank]): ValueKind = {
      require(cards.size === 5, s"Only 5 card evaluations allowed: $cards")

      val rankList: List[(Rank, Int)] = cards
        .groupBy(identity)
        .toList
        .map { case (k, v) =>
          (k, v.size)
        }
        .sortBy { case (k, v) =>
          (v, k.strength)
        }
        .reverse

      val rankCounts: List[Int] = rankList
        .map { case (_, v) => v }

      val mapping = Map(
        (5 :: Nil)                     -> ValueKind.FiveOfAKind,
        (4 :: 1 :: Nil)                -> ValueKind.FourOfAKind,
        (3 :: 2 :: Nil)                -> ValueKind.FullHouse,
        (3 :: 1 :: 1 :: Nil)           -> ValueKind.ThreeOfAKind,
        (2 :: 2 :: 1 :: Nil)           -> ValueKind.TwoPairs,
        (2 :: 1 :: 1 :: 1 :: Nil)      -> ValueKind.Pair,
        (1 :: 1 :: 1 :: 1 :: 1 :: Nil) -> ValueKind.HighCard,
      )

      mapping.getOrElse(
        rankCounts,
        sys.error(s"Unrecognized rank counts $rankCounts for $cards"),
      )
    }

    implicit val ordering: Ordering[Value] = (x: Value, y: Value) => {
      val result = Ordering[Int].compare(x.kind.strength, y.kind.strength)

      if (result === 0) {
        Ordering[List[Rank]].compare(
          x.originalRankList,
          y.originalRankList,
        )
      } else {
        result // major decides
      }
    }

    def apply(pokerGame: PokerGame, hand: Hand): Value =
      pokerGame match {
        case PokerGame.Camel1 =>
          Value(Value.determineKind(hand.ranks), hand.ranks)

        case PokerGame.Camel2 =>
          val ranks = hand.ranks.map { r =>
            if (r == Rank('J')) Rank.Wildcard else r
          }

          def expandWildcards(
            hand: List[Rank]
          ): List[List[Rank]] = {
            def f(
              cards: List[Rank]
            ): List[List[Rank]] =
              cards match {
                case h :: t if h == Rank.Wildcard =>
                  Rank.NonWildCardRanks flatMap { r =>
                    f(t) map { x => r :: x }
                  }

                case h :: t => f(t) map { x => h :: x }
                case Nil    => Nil :: Nil
              }

            f(hand)
          }

          val options   = expandWildcards(ranks)
          val bestValue = options
            .map(Value.determineKind)
            .max((x: ValueKind, y: ValueKind) => x.strength.compare(y.strength))

          Value(bestValue, ranks)
      }

  }

  sealed abstract class ValueKind(val strength: Int)

  object ValueKind {
    final case object HighCard     extends ValueKind(0)
    final case object Pair         extends ValueKind(1)
    final case object TwoPairs     extends ValueKind(2)
    final case object ThreeOfAKind extends ValueKind(3)
    final case object FullHouse    extends ValueKind(4)
    final case object FourOfAKind  extends ValueKind(5)
    final case object FiveOfAKind  extends ValueKind(6)
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
    input.parseLines(HandWithBid.parse)

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
