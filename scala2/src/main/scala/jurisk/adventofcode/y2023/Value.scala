package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent07.{Hand, PokerGame, Rank}

import scala.annotation.tailrec

final case class Value(kind: ValueKind, originalRankList: List[Rank])

sealed abstract class ValueKind(val strength: Int)

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

    if (rankCounts === 5 :: Nil) {
      FiveOfAKind
    } else if (rankCounts === 4 :: 1 :: Nil) {
      FourOfAKind
    } else if (rankCounts === 3 :: 2 :: Nil) {
      FullHouse
    } else if (rankCounts === 3 :: 1 :: 1 :: Nil) {
      ThreeOfAKind
    } else if (rankCounts === 2 :: 2 :: 1 :: Nil) {
      TwoPairs
    } else if (rankCounts === 2 :: 1 :: 1 :: 1 :: Nil) {
      Pair
    } else if (rankCounts === 1 :: 1 :: 1 :: 1 :: 1 :: Nil) {
      HighCard
    } else {
      sys.error(s"Unrecognized value $cards")
    }
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

  final case object HighCard     extends ValueKind(0)
  final case object Pair         extends ValueKind(1)
  final case object TwoPairs     extends ValueKind(2)
  final case object ThreeOfAKind extends ValueKind(3)
  final case object FullHouse    extends ValueKind(4)
  final case object FourOfAKind  extends ValueKind(5)
  final case object FiveOfAKind  extends ValueKind(6)
}
