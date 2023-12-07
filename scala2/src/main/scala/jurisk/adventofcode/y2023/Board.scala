package jurisk.adventofcode.y2023

import cats.implicits._

case class Board(cards: List[Card]) {
  override def toString: String = cards.map(_.toString).sorted.mkString
}

object Board {
  def apply(cards: Card*): Board = {
    assert(cards.size == 5, "Expected 5 board cards")
    new Board(List(cards: _*))
  }

  def parse(x: String): Board = {
    val cards = Card.parseSequence(x)
    require(cards.size === 5, s"Wrong number of board cards: $cards")
    Board(cards)
  }
}
