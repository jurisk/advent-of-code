package jurisk.adventofcode.y2023

sealed trait PokerGame {
  def cardsOnBoard: Int
  def cardsPerHand: Int
  def name: String
  def capitalizedName: String

  override def toString: String = name
}

object PokerGame {
  val all: Set[PokerGame] = Set(TexasHoldem, OmahaHoldem, FiveCardDraw, Camel)

  def parse(x: String): PokerGame = x match {
    case TexasHoldem.name  => TexasHoldem
    case OmahaHoldem.name  => OmahaHoldem
    case FiveCardDraw.name => FiveCardDraw
    case x                 => sys.error(s"Didn't recognize $x")
  }

  final case object TexasHoldem extends PokerGame {
    val cardsOnBoard: Int       = 5
    val cardsPerHand: Int       = 2
    val name                    = "texas-holdem"
    val capitalizedName: String = "TexasHoldem"
  }

  final case object OmahaHoldem extends PokerGame {
    val cardsOnBoard: Int       = 5
    val cardsPerHand: Int       = 4
    val name                    = "omaha-holdem"
    val capitalizedName: String = "OmahaHoldem"
  }

  final case object FiveCardDraw extends PokerGame {
    val cardsOnBoard: Int       = 0
    val cardsPerHand: Int       = 5
    val name                    = "five-card-draw"
    val capitalizedName: String = "FiveCardDraw"
  }

  final case object Camel extends PokerGame {
    val cardsOnBoard: Int       = 0
    val cardsPerHand: Int       = 5
    val name                    = "camel"
    val capitalizedName: String = "Camel"

  }
}
