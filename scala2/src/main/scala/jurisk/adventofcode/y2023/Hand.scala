package jurisk.adventofcode.y2023

case class Hand(cards: List[Rank]) {
  override def toString: String = cards.map(_.toString).mkString
}

object Hand {
  def parse(x: String): Hand =
    Hand((x map Rank.parse).toList)
}
