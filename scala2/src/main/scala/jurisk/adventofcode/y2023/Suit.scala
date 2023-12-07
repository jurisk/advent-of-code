package jurisk.adventofcode.y2023

import cats.implicits._

sealed abstract class Suit private (val value: Char) {
  override def toString: String = value.toString
}

object Suit {
  case object Clubs    extends Suit('c')
  case object Hearts   extends Suit('h')
  case object Spades   extends Suit('s')
  case object Diamonds extends Suit('d')

  val valid: Set[Suit] = Set(
    Clubs,
    Hearts,
    Spades,
    Diamonds,
  )

  def parse(x: Char): Suit =
    valid.find(_.value === x).getOrElse(sys.error(s"Failed to parse suit $x"))
}
