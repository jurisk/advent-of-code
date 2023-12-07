package jurisk.adventofcode.y2023

sealed trait PokerGame {
  def name: String
  override def toString: String = name
}

object PokerGame {
  final case object Camel1 extends PokerGame {
    val name                    = "camel-1"
  }

  final case object Camel2 extends PokerGame {
    val name = "camel-2"
  }
}
