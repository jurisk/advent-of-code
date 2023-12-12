package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import Ordering.Implicits.seqOrdering

object Advent07 {
  type Input = List[HandWithBid]

  sealed trait PokerGame {
    def handValue(hand: Hand): Value
  }

  object PokerGame {
    case object Camel1 extends PokerGame {
      override def handValue(hand: Hand): Value =
        Value(ValueKind.fromRanks(hand.ranks), hand.ranks)
    }

    case object Camel2 extends PokerGame {
      override def handValue(hand: Hand): Value = {
        val ranks = hand.ranks map { r =>
          if (r == Rank.parse('J')) Rank.Wildcard else r
        }

        val wildcards              = ranks.count(_ == Rank.Wildcard)
        val withoutWildcards       = ranks.filterNot(_ == Rank.Wildcard)
        val withoutWildcardsCounts = Rank.rankCounts(withoutWildcards)

        val withWildcardsCounts = withoutWildcardsCounts match {
          case h :: t => (h + wildcards) :: t
          case Nil    => wildcards :: Nil
        }

        val kind = ValueKind.fromCounts(withWildcardsCounts)

        Value(kind, ranks)
      }
    }
  }

  final case class Rank(value: Char, strength: Int)

  object Rank {
    implicit val ordering: Ordering[Rank] = Ordering.by(_.strength)

    val Ordered: List[Rank] = "*23456789TJQKA".toList.zipWithIndex.map {
      case (ch, idx) =>
        Rank(ch, idx)
    }

    val Wildcard: Rank = Rank.parse('*')

    def parse(x: Char): Rank =
      Ordered
        .find(_.value === x)
        .getOrElse(x.toString.failedToParse("Rank"))

    def rankCounts(ranks: List[Rank]): List[Int] =
      ranks.counts.values.toList
        .sorted(Ordering[Int].reverse)
  }

  final case class Hand(ranks: List[Rank])

  object Hand {
    def parse(x: String): Hand =
      Hand((x map Rank.parse).toList)
  }

  final case class Value(kind: ValueKind, ranks: List[Rank])

  object Value {
    def orderingForGame(pokerGame: PokerGame): Ordering[Hand] =
      Ordering.by(pokerGame.handValue)

    implicit val ordering: Ordering[Value] =
      Ordering.by[Value, ValueKind](_.kind) orElse Ordering.by(_.ranks)
  }

  sealed abstract class ValueKind(val strength: Int)

  object ValueKind {
    implicit val ordering: Ordering[ValueKind] = Ordering.by(_.strength)

    case object HighCard     extends ValueKind(0)
    case object Pair         extends ValueKind(1)
    case object TwoPairs     extends ValueKind(2)
    case object ThreeOfAKind extends ValueKind(3)
    case object FullHouse    extends ValueKind(4)
    case object FourOfAKind  extends ValueKind(5)
    case object FiveOfAKind  extends ValueKind(6)

    private val Mapping = Map(
      (5 :: Nil)                     -> ValueKind.FiveOfAKind,
      (4 :: 1 :: Nil)                -> ValueKind.FourOfAKind,
      (3 :: 2 :: Nil)                -> ValueKind.FullHouse,
      (3 :: 1 :: 1 :: Nil)           -> ValueKind.ThreeOfAKind,
      (2 :: 2 :: 1 :: Nil)           -> ValueKind.TwoPairs,
      (2 :: 1 :: 1 :: 1 :: Nil)      -> ValueKind.Pair,
      (1 :: 1 :: 1 :: 1 :: 1 :: Nil) -> ValueKind.HighCard,
    )

    def fromCounts(counts: List[Int]): ValueKind =
      Mapping.getOrElse(
        counts,
        s"Unrecognized rank counts $counts".fail,
      )

    def fromRanks(ranks: List[Rank]): ValueKind = {
      require(ranks.size === 5, s"Only 5 card evaluations are allowed: $ranks")

      val counts = Rank.rankCounts(ranks)
      fromCounts(counts)
    }
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
