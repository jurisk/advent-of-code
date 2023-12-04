package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent04 {
  type Parsed         = List[Card]
  private type CardId = Int

  final case class Card(
    id: CardId,
    winningNumbers: Set[Int],
    numbersYouHave: Set[Int],
  ) {
    val matchingNumbers: Int =
      winningNumbers.intersect(numbersYouHave).size
  }

  private object Card {
    def parse(s: String): Card =
      s match {
        case s"Card$id: $winning | $youHave" =>
          Card(
            id.trim.toInt,
            winning.extractInts.toSet,
            youHave.extractInts.toSet,
          )
        case _                               => s.failedToParse
      }
  }

  def parse(input: String): Parsed =
    input.parseList("\n", Card.parse)

  def part1(cards: Parsed): Int = {
    def part1Worth(card: Card): Int =
      Math.pow(2, card.matchingNumbers - 1).toInt

    cards.map(part1Worth).sum
  }

  def part2(cards: Parsed): Int = {
    val validIds: Set[CardId] = cards.map(_.id).toSet

    def cardsWon(card: Card): List[CardId] =
      (card.id + 1 until card.id + 1 + card.matchingNumbers)
        .filter(validIds.contains)
        .toList

    val cardMapping = cards.foldLeft(Map.empty[CardId, List[CardId]]) {
      case (acc, card) =>
        acc + (card.id -> cardsWon(card))
    }

    def count(cardId: CardId): Int =
      1 + cardMapping(cardId).map(x => count(x)).sum

    cards.map(x => count(x.id)).sum
  }

  def parseFile(fileName: String): Parsed =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Parsed = parseFile("2023/04.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
