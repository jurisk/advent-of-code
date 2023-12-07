package jurisk.adventofcode.y2023

import cats.implicits._

case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"$rank$suit"
}

object Card {
  private val RepresentationLength = 1
//  private val RepresentationLength = 2

  def parse(x: String): Card = {
    require(
      x.length === RepresentationLength,
      s"$x should be $RepresentationLength characters",
    )
    Card(
      Rank.parse(x.head),
      Suit.Spades,
//      Suit.parse(x(1))
    )
  }

  def parseSequence(x: String): List[Card] =
    x.grouped(Card.RepresentationLength).map(Card.parse).toList

  val deck: Set[Card] = (Rank.ordered, Suit.valid.toList).mapN(Card(_, _)).toSet
}
