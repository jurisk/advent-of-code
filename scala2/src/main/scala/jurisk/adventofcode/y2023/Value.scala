package jurisk.adventofcode.y2023

import cats.implicits._

import scala.annotation.tailrec

sealed abstract class Value(val major: Int) {
  def rankList: List[Rank]
  def originalRankList: List[Rank]
}

object Value {
  private def isFlush(cards: Set[Card]): Boolean = cards.map(_.suit).size === 1

  private def isStraight(cards: Set[Card]): Option[Rank] = {
    val ranks = cards.toList.map(_.rank)
    import Rank._
    if (ranks.toSet === Set(Ace, Two, Three, Four, Five)) { // so-called "wheel" straight
      Five.some                                             // is considered five-high
    } else {
      val intValues = ranks.map(_.strength).sorted
      val result    = (intValues.init zip intValues.tail) forall { case (a, b) =>
        a + 1 === b
      }
      if (result) ranks.maxBy(_.strength).some
      else none
    }
  }

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

  def orderingForBoard(
    pokerGame: PokerGame,
  ): Ordering[Hand] = (x: Hand, y: Hand) =>
    ordering.compare(
      Value(pokerGame, x),
      Value(pokerGame, y),
    )

  def apply(cards: List[Card]): Value = {
    require(cards.size === 5, s"Only 5 card evaluations allowed: $cards")

    val originalRankList = cards.map(_.rank)

    val rankList: List[(Rank, Int)] = cards
      .groupBy(_.rank)
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

//    val straight = isStraight(cards)

//    if (isFlush(cards) && straight.isDefined) {
//      StraightFlush(straight.getOrElse(sys.error("Shouldn't ever happen")))
//    } else

    if (rankCounts === 5 :: Nil) {
      FiveOfAKind(uniqueRanks.head, originalRankList)
    } else if (rankCounts === 4 :: 1 :: Nil) {
      FourOfAKind(uniqueRanks.head, uniqueRanks(1), originalRankList)
    } else if (rankCounts === 3 :: 2 :: Nil) {
      FullHouse(uniqueRanks.head, uniqueRanks(1), originalRankList)
//    } else if (isFlush(cards)) {
//      Flush(uniqueRanks.toSet)
//    } else if (straight.isDefined) {
//      Straight(straight.getOrElse(sys.error("Shouldn't ever happen")))
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

  private def allNCardSubsets(N: Int, cards: Set[Card]): List[Set[Card]] = {
    def f(list: List[Card]): List[Set[Card]] =
      if (list.length < N) {
        sys.error(s"This should not be happening: $N $cards")
      } else if (list.length === N) {
        list.toSet :: Nil
      } else {
        list flatMap { x =>
          val without = list.filterNot(_ == x)
          f(without)
        }
      }

    f(cards.toList)
  }

  implicit val ordering: Ordering[Value] = (x: Value, y: Value) => {
    val result = Ordering[Int].compare(x.major, y.major)

    if (result === 0) {
      Ordering[List[Rank]].compare(
        x.originalRankList,
        y.originalRankList,
      ) // Note - this assumes reverse ordering obtained from .rankList
//      Ordering[List[Rank]].compare(x.rankList, y.rankList) // Note - this assumes reverse ordering obtained from .rankList
    } else {
      result // major decides
    }
  }

  def apply(pokerGame: PokerGame, hand: Hand): Value =
    pokerGame match {
      case PokerGame.Camel1 =>
        Value(hand.cards)

      case PokerGame.Camel2 => {
        def f(cards: List[Rank], wildCard: Rank, ranks: List[Rank]): List[List[Rank]] = {
          cards match {
            case h :: t if h == wildCard =>
              ranks flatMap { r =>
                f(t, wildCard, ranks).map { x =>
                  r :: x
                }
              }

            case h :: t =>
              f(t, wildCard, ranks).map { x =>
                h :: x
              }
            case Nil => Nil :: Nil
          }
        }

        def expandWildCards(hand: Hand, wildCard: Rank): List[Hand] = {
          val ranks = Rank.ordered.filterNot(_ == wildCard)
          val results = f(hand.cards.map(_.rank), wildCard, ranks)
//          println(s"Expand $hand => $results")
          results.map { x =>
            Hand(x.map(r => Card(r, Suit.Spades)))
          }
        }

        val options: List[Hand] = expandWildCards(hand, Rank.Jack)
        val bestValue = options.map(x => Value(x.cards)).max(new Ordering[Value] {
          override def compare(x: Value, y: Value): Int = x.major.compare(y.major)
        })

        val ranks = hand.cards.map(_.rank).map { r =>
          if (r == Rank.Jack) {
            Rank.Worst
          } else {
            r
          }
        }

        bestValue match {
          case x: HighCard => x.copy(originalRankList = ranks)
          case x: Pair => x.copy(originalRankList = ranks)
          case x: TwoPairs => x.copy(originalRankList = ranks)
          case x: ThreeOfAKind => x.copy(originalRankList = ranks)
          case x: FullHouse => x.copy(originalRankList = ranks)
          case x: FourOfAKind => x.copy(originalRankList = ranks)
          case x: FiveOfAKind => x.copy(originalRankList = ranks)
        }
      }
    }

  private def kickers(x: Set[Rank]): String =
    "kickers " + x.toList.sorted.reverse.map(_.toString).mkString("-")

  case class HighCard(ranks: Set[Rank], originalRankList: List[Rank])
      extends Value(0) {
    override def rankList: List[Rank] = ranks.toList.sorted.reverse
    override def toString: String     = s"High Card, ${kickers(ranks)}"
  }

  case class Pair(two: Rank, others: Set[Rank], originalRankList: List[Rank])
      extends Value(1) {
    override def rankList: List[Rank] = two :: others.toList.sorted.reverse
    override def toString: String     = s"Pair of $two, ${kickers(others)}"
  }

  case class TwoPairs(
    twoHigh: Rank,
    twoLow: Rank,
    remaining: Rank,
    originalRankList: List[Rank],
  ) extends Value(2) {
    override def rankList: List[Rank] = twoHigh :: twoLow :: remaining :: Nil
    override def toString: String     =
      s"Two pairs, $twoHigh and $twoLow, kicker $remaining"
  }

  case class ThreeOfAKind(
    three: Rank,
    others: Set[Rank],
    originalRankList: List[Rank],
  ) extends Value(3) {
    override def rankList: List[Rank] = three :: others.toList.sorted.reverse
    override def toString: String     =
      s"Three of a Kind of $three, ${kickers(others)}"
  }

//  case class Straight(highest: Rank) extends Value(4) {
//    override def rankList: List[Rank] = highest :: Nil
//    override def toString: String = s"Straight starting with $highest"
//  }
//
//  case class Flush(ranks: Set[Rank]) extends Value(5) {
//    override def rankList: List[Rank] = ranks.toList.sorted.reverse
//    override def toString: String = s"Flush, ${kickers(ranks)}"
//  }

  case class FullHouse(three: Rank, two: Rank, originalRankList: List[Rank])
      extends Value(6) {
    override def rankList: List[Rank] = three :: two :: Nil
    override def toString: String     = s"Full-House of $three and $two"
  }

  case class FourOfAKind(four: Rank, one: Rank, originalRankList: List[Rank])
      extends Value(7) {
    override def rankList: List[Rank] = four :: one :: Nil
    override def toString: String     = s"Four of a kind of $four, kicker $one"
  }

//  case class StraightFlush(highest: Rank) extends Value(8) {
//    override def rankList: List[Rank] = highest :: Nil
//    override def toString: String = s"Straight Flush starting with $highest"
//  }

  case class FiveOfAKind(five: Rank, originalRankList: List[Rank])
      extends Value(9) {
    override def rankList: List[Rank] = five :: Nil

    override def toString: String = s"Five of a kind of $five"
  }

}
