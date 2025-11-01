package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.AdventApp
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.y2020.Advent22.Game

import scala.annotation.tailrec

object Advent22 extends AdventApp[Game, Long, Long]:
  override val year: Card     = 2020
  override val exercise: Card = 22

  opaque type Card = Int
  opaque type Deck = List[Card]

  enum Player:
    case A
    case B

  private def score(deck: Deck): Long = deck.zipWithIndex.foldLeft(0L) {
    case (acc, (card, idx)) =>
      acc + card * (deck.length - idx)
  }

  final case class Game(deckA: Deck, deckB: Deck, history: Set[(Deck, Deck)]):
    private def compare(cardA: Card, cardB: Card): Player =
      if cardA > cardB then Player.A
      else if cardB > cardA then Player.B
      else sys.error("Didn't expect to have to compare equal cards")

    private def cycle(winner: Player): Game = winner match
      case Player.A =>
        Game(
          deckA.tail ++ List(deckA.head, deckB.head),
          deckB.tail,
          history + ((deckA, deckB)),
        )

      case Player.B =>
        Game(
          deckA.tail,
          deckB.tail ++ List(deckB.head, deckA.head),
          history + ((deckA, deckB)),
        )

    def recursiveGame: (Player, Deck) =
      if history.contains((deckA, deckB))
      then (Player.A, deckA)
      else
        (deckA, deckB) match
          case (Nil, Nil)                       => sys.error("Both decks cannot be empty")
          case (deckA, Nil)                     => (Player.A, deckA)
          case (Nil, deckB)                     => (Player.B, deckB)
          case (headA :: tailA, headB :: tailB) =>
            if headA <= tailA.length && headB <= tailB.length
            then
              val (winnerRecursive, _) = Game(
                tailA.take(headA),
                tailB.take(headB),
                Set.empty,
              ).recursiveGame
              cycle(winnerRecursive).recursiveGame
            else cycle(compare(headA, headB)).recursiveGame

    @tailrec
    def simpleGame: Deck = (deckA, deckB) match
      case (deckA, Nil)             => deckA
      case (Nil, deckB)             => deckB
      case (headA :: _, headB :: _) => cycle(compare(headA, headB)).simpleGame

  override def parseInput(lines: Iterator[String]): Either[ErrorMessage, Game] =
    val decks: List[Deck] = lines
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toList)
      .map(_.tail.map(_.toInt))
      .toList

    decks match
      case a :: b :: Nil => Game(a, b, Set.empty).asRight
      case _             => ErrorMessage.left(s"Failed to read: $decks")

  override def solution1(input: Game): Long =
    score(input.simpleGame)

  override def solution2(input: Game): Long =
    val (_, winner2) = input.recursiveGame
    score(winner2)
