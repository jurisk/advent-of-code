package jurisk.adventofcode.y2023

case class Hand(ranks: List[Rank]) {
  override def toString: String = ranks.map(_.toString).mkString
}

object Hand {
  def parse(x: String): Hand =
    Hand((x map Rank.parse).toList)
}
