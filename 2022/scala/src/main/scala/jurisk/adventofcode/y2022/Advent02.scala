package jurisk.adventofcode.y2022

import jurisk.adventofcode.y2022.Advent02.Outcome._
import jurisk.utils.FileInput.readFileLines
import org.scalatest.matchers.should.Matchers._

object Advent02 {
  type Result  = Int
  type Parsed1 = List[Round1]
  type Parsed2 = List[Round2]

  sealed trait Selection {
    def score: Result
    def losesAgainst: Selection
    def winsOver: Selection
    def outcomeAgainst(opponent: Selection): Outcome =
      if (winsOver == opponent) Win
      else if (losesAgainst == opponent) Loss
      else Draw
  }

  object Selection {
    case object Rock extends Selection {
      override def score: Result           = 1
      override def losesAgainst: Selection = Paper
      override def winsOver: Selection     = Scissors
    }

    case object Paper extends Selection {
      override def score: Result           = 2
      override def losesAgainst: Selection = Scissors
      override def winsOver: Selection     = Rock
    }

    case object Scissors extends Selection {
      override def score: Result           = 3
      override def losesAgainst: Selection = Rock
      override def winsOver: Selection     = Paper
    }

    def parse(s: String): Selection =
      s match {
        case "A" | "X" => Rock
        case "B" | "Y" => Paper
        case "C" | "Z" => Scissors
        case _         => sys.error(s"Unrecognized selection $s")
      }
  }

  sealed trait Outcome {
    def score: Result
  }

  object Outcome {
    case object Win extends Outcome {
      override def score: Result = 6
    }

    case object Draw extends Outcome {
      override def score: Result = 3
    }

    case object Loss extends Outcome {
      override def score: Result = 0
    }

    // X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
    def parse(s: String): Outcome =
      s match {
        case "X" => Loss
        case "Y" => Draw
        case "Z" => Win
        case _   => sys.error(s"Unrecognized outcome $s")
      }
  }

  final case class Round1(
    you: Selection,
    opponent: Selection,
  ) {
    def result: Result = you.score + you.outcomeAgainst(opponent).score
  }

  object Round1 {
    def parse(s: String): Round1 = {
      val Array(o, y) = s.split(" ")

      val you      = Selection.parse(y)
      val opponent = Selection.parse(o)

      Round1(you, opponent)
    }
  }

  final case class Round2(
    opponent: Selection,
    outcomeForYou: Outcome,
  ) {
    def toRound1: Round1 = {
      val you = outcomeForYou match {
        case Outcome.Win  => opponent.losesAgainst
        case Outcome.Draw => opponent
        case Outcome.Loss => opponent.winsOver
      }

      Round1(you, opponent)
    }
  }

  object Round2 {
    def parse(s: String): Round2 = {
      val Array(o, w) = s.split(" ")

      val opponent      = Selection.parse(o)
      val outcomeForYou = Outcome.parse(w)

      Round2(opponent, outcomeForYou)
    }
  }

  private def parse1(fileName: String): Parsed1 =
    readFileLines(fileName) map Round1.parse

  private def parse2(fileName: String): Parsed2 =
    readFileLines(fileName) map Round2.parse

  def part1(data: Parsed1): Result =
    data.map(_.result).sum

  def part2(data: Parsed2): Result =
    data.map(_.toRound1.result).sum

  def main(args: Array[String]): Unit = {
    val test1 = parse1("02-test.txt")
    val real1 = parse1("02.txt")

    part1(test1) shouldEqual 15
    part1(real1) shouldEqual 14531

    val test2 = parse2("02-test.txt")
    val real2 = parse2("02.txt")

    part2(test2) shouldEqual 12
    part2(real2) shouldEqual 11258
  }
}
