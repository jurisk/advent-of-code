package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp
import cats.implicits._

final case class Item(
  a: Int,
  b: Int,
  letter: Char,
  password: String,
):
  def isValid1: Boolean =
    val count = password.count(_ == letter)
    (count >= a) && (count <= b)

  def isValid2: Boolean =
    List(password(a - 1), password(b - 1)).count(_ == letter) == 1

object Advent02 extends SingleLineAdventApp[Item, Int]:
  val year: Int = 2020
  val exercise: Int = 2

  override def parseLine(line: String): Either[ErrorMessage, Item] = {
    val Pattern = """(\d+)-(\d+) (\w): (\w+)""".r

    line match
      case Pattern(a, b, letter, password) if letter.length == 1 =>
        Item(a.toInt, b.toInt, letter.head, password).asRight
      case _ =>
        ErrorMessage(s"Couldn't parse $line").asLeft
  }

  override def solution1(input: List[Item]): Int = input.count(_.isValid1)

  override def solution2(input: List[Item]): Int = input.count(_.isValid2)
