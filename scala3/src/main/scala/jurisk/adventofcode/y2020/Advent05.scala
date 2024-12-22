package jurisk.adventofcode.y2020

import cats.effect.*
import cats.implicits.*
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp

import scala.io.Source

object Advent05 extends SingleLineAdventApp[Int, Int, Int]:
  override val year: Int = 2020
  override val exercise: Int = 5

  private def decode(zero: Char, one: Char)(input: String): Either[ErrorMessage, Int] =
    input.map {
        case `zero` => 0.asRight
        case `one` => 1.asRight
        case c => ErrorMessage(s"Unexpected character $c in $input").asLeft
      }
      .toList
      .sequence
      .map(_.foldLeft(0) { case (acc, x) => acc * 2 + x })

  private[y2020] def seatId(x: String): Either[ErrorMessage, Int] =
    for
      encoded <- if x.length == 10
      then x.splitAt(7).asRight
      else ErrorMessage(s"Invalid input $x").asLeft
      (encodedRow, encodedColumn) = encoded
      row <- decode('F', 'B')(encodedRow)
      column <- decode('L', 'R')(encodedColumn)
    yield row * 8 + column

  private def findLast(sortedSeatIds: List[Int]): Int =
    sortedSeatIds.lastOption getOrElse sys.error("List was empty")

  private def findFree(sortedSeatIds: List[Int]): Int =
    (sortedSeatIds.init zip sortedSeatIds.tail)
      .find { case (a, b) =>
        b - a == 2
      }
      .map { case (a, _) =>
        a + 1
      }
      .getOrElse(sys.error(s"Didn't find a free seat in $sortedSeatIds"))

  private def solve(lines: List[Int], f: List[Int] => Int): Int =
    f(lines.sorted)

  override def parseLine(line: String): Either[ErrorMessage, Int] = seatId(line)

  override def solution1(input: List[Int]): Int = solve(input, findLast)

  override def solution2(input: List[Int]): Int = solve(input, findFree)
