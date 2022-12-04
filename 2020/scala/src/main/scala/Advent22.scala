import scala.annotation.tailrec
import scala.io.Source

object Advent22 extends App:
  opaque type Card = Int
  opaque type Deck = List[Card]

  enum Player:
    case A
    case B

  private def score(deck: Deck): Long = deck.zipWithIndex.foldLeft(0L) { case (acc, (card, idx)) =>
    acc + card * (deck.length - idx)
  }

  final case class Game(deckA: Deck, deckB: Deck, history: Set[(Deck, Deck)]):
    private def compare(cardA: Card, cardB: Card): Player =
      if cardA > cardB then Player.A
      else if cardB > cardA then Player.B
      else sys.error("Didn't expect to have to compare equal cards")

    private def cycle(winner: Player): Game = winner match
      case Player.A => Game(
        deckA.tail ++ List(deckA.head, deckB.head),
        deckB.tail,
        history + ((deckA, deckB)),
      )

      case Player.B => Game(
        deckA.tail,
        deckB.tail ++ List(deckB.head, deckA.head),
        history + ((deckA, deckB)),
      )

    def recursiveGame: (Player, Deck) =
      if history.contains((deckA, deckB))
      then (Player.A, deckA)
      else (deckA, deckB) match
        case (Nil, Nil) => sys.error("Both decks cannot be empty")
        case (deckA, Nil) => (Player.A, deckA)
        case (Nil, deckB) => (Player.B, deckB)
        case (headA :: tailA, headB :: tailB) =>
          if headA <= tailA.length && headB <= tailB.length
          then
            val (winnerRecursive, _) = Game(tailA.take(headA), tailB.take(headB), Set.empty).recursiveGame
            cycle(winnerRecursive).recursiveGame
          else cycle(compare(headA, headB)).recursiveGame

    @tailrec
    def simpleGame: Deck = (deckA, deckB) match
      case (deckA, Nil) => deckA
      case (Nil, deckB) => deckB
      case (headA :: _, headB :: _) => cycle(compare(headA, headB)).simpleGame
      case (Nil, Nil) => sys.error(s"Unexpected empty decks")

  private def readGame(fileName: String): Game =
    val decks: List[Deck] = Source
      .fromResource(fileName)
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toList)
      .map(_.tail.map(_.toInt))
      .toList

    decks match
      case a :: b :: Nil => Game(a, b, Set.empty)
      case _ => sys.error(s"Failed to read: $decks")

  def run(fileName: String, expected1: Long, expected2: Long): Unit =
    val game = readGame(fileName)
    val winner1 = game.simpleGame
    val solution1 = score(winner1)
    println(solution1)
    assert(solution1 == expected1)

    val (_, winner2) = game.recursiveGame
    val solution2 = score(winner2)
    println(solution2)
    assert(solution2 == expected2)

  run("22-test.txt", 306, 291)
  run("22.txt", 34566, 31854)

  println("Passed")
