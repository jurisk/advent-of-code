package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent07.{Hand, PokerGame}

import scala.annotation.tailrec

sealed abstract class Value(val major: Int) {
  def originalRankList: List[Rank]
}

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

  def apply(cards: List[Rank]): Value = {
    require(cards.size === 5, s"Only 5 card evaluations allowed: $cards")

    val originalRankList = cards

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

    val uniqueRanks: List[Rank] = rankList
      .map { case (k, _) => k }

    val rankCounts: List[Int] = rankList
      .map { case (_, v) => v }

    assert(uniqueRanks.length === rankCounts.length)

    if (rankCounts === 5 :: Nil) {
      FiveOfAKind(uniqueRanks.head, originalRankList)
    } else if (rankCounts === 4 :: 1 :: Nil) {
      FourOfAKind(uniqueRanks.head, uniqueRanks(1), originalRankList)
    } else if (rankCounts === 3 :: 2 :: Nil) {
      FullHouse(uniqueRanks.head, uniqueRanks(1), originalRankList)
    } else if (rankCounts === 3 :: 1 :: 1 :: Nil) {
      ThreeOfAKind(
        uniqueRanks.head,
        uniqueRanks.filterNot(_ == uniqueRanks.head).toSet,
        originalRankList,
      )
    } else if (rankCounts === 2 :: 2 :: 1 :: Nil) {
      TwoPairs(
        uniqueRanks.head,
        uniqueRanks(1),
        uniqueRanks(2),
        originalRankList,
      )
    } else if (rankCounts === 2 :: 1 :: 1 :: 1 :: Nil) {
      Pair(
        uniqueRanks.head,
        uniqueRanks.filterNot(_ == uniqueRanks.head).toSet,
        originalRankList,
      )
    } else if (rankCounts === 1 :: 1 :: 1 :: 1 :: 1 :: Nil) {
      HighCard(uniqueRanks.toSet, originalRankList)
    } else {
      sys.error(s"Unrecognized value $cards")
    }
  }

  implicit val ordering: Ordering[Value] = (x: Value, y: Value) => {
    val result = Ordering[Int].compare(x.major, y.major)

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
        Value(hand.ranks)

      case PokerGame.Camel2 =>
        def expandWildCards(hand: Hand, wildCard: Rank): List[Hand] = {
          val nonWildCardRanks = Rank.ordered.filterNot(_ == wildCard)

          def f(
            cards: List[Rank]
          ): List[List[Rank]] =
            cards match {
              case h :: t if h == wildCard =>
                nonWildCardRanks flatMap { r =>
                  f(t) map { x => r :: x }
                }

              case h :: t => f(t) map { x => h :: x }
              case Nil    => Nil :: Nil
            }

          val results = f(hand.ranks)
          results.map(Hand(_))
        }

        val options: List[Hand] = expandWildCards(hand, Rank.Jack)
        val bestValue           = options
          .map(x => Value(x.ranks))
          .max((x: Value, y: Value) => x.major.compare(y.major))

        val ranks = hand.ranks.map { r =>
          if (r == Rank.Jack) {
            Rank.Worst
          } else {
            r
          }
        }

        bestValue match {
          case x: HighCard     => x.copy(originalRankList = ranks)
          case x: Pair         => x.copy(originalRankList = ranks)
          case x: TwoPairs     => x.copy(originalRankList = ranks)
          case x: ThreeOfAKind => x.copy(originalRankList = ranks)
          case x: FullHouse    => x.copy(originalRankList = ranks)
          case x: FourOfAKind  => x.copy(originalRankList = ranks)
          case x: FiveOfAKind  => x.copy(originalRankList = ranks)
        }
    }

  case class HighCard(ranks: Set[Rank], originalRankList: List[Rank])
      extends Value(0) {}

  case class Pair(two: Rank, others: Set[Rank], originalRankList: List[Rank])
      extends Value(1) {}

  case class TwoPairs(
    twoHigh: Rank,
    twoLow: Rank,
    remaining: Rank,
    originalRankList: List[Rank],
  ) extends Value(2) {
    override def toString: String =
      s"Two pairs, $twoHigh and $twoLow, kicker $remaining"
  }

  case class ThreeOfAKind(
    three: Rank,
    others: Set[Rank],
    originalRankList: List[Rank],
  ) extends Value(3) {}

  case class FullHouse(three: Rank, two: Rank, originalRankList: List[Rank])
      extends Value(6) {
    override def toString: String = s"Full-House of $three and $two"
  }

  case class FourOfAKind(four: Rank, one: Rank, originalRankList: List[Rank])
      extends Value(7) {
    override def toString: String = s"Four of a kind of $four, kicker $one"
  }

  case class FiveOfAKind(five: Rank, originalRankList: List[Rank])
      extends Value(9) {
    override def toString: String = s"Five of a kind of $five"
  }
}
