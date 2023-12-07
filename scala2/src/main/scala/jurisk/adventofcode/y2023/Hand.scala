package jurisk.adventofcode.y2023

import cats.implicits._

case class Hand(cards: List[Card]) {
  override def toString: String = cards.map(_.toString).mkString
}

object Hand {
  def apply(cards: Card*): Hand = new Hand(List(cards: _*))

  def parse(x: String): Hand = {
    val cards = Card.parseSequence(x)
    Hand(cards)
  }

  def sortHands(set: Set[Hand]): List[Hand] = set.toList.sortBy(_.toString)

  def render(hands: List[Hand]): String =
    hands.map(_.toString).mkString(" ")
}
