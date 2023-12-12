package jurisk.adventofcode.y2023

import jurisk.math.QuadraticEquation
import jurisk.math.countLongsBetweenExclusive

object Advent06 {
  type Input = List[Race]

  final case class Race(
    timeBudget: Long,
    distanceToBeat: Long,
  ) {
    def waysToWinAlgebraic: Long = {
      val equation =
        QuadraticEquation(-1, timeBudget.toDouble, -distanceToBeat.toDouble)

      equation.roots match {
        case a :: b :: Nil => countLongsBetweenExclusive(a, b)
        case _             => 0
      }
    }

    def waysToWinBruteForce: Long =
      (0 to timeBudget.toInt)
        .map { buttonHeld =>
          buttonHeld * (timeBudget - buttonHeld)
        }
        .count(_ > distanceToBeat)
  }

  def solve(data: Input): Long =
    data.map(_.waysToWinAlgebraic).product

  def convertInput(input: Input): Input = {
    val (timeBudgetStr, distanceToBeatStr) = input.foldLeft(("", "")) {
      case ((timeBudgetAcc, distanceToBeatAcc), race) =>
        (
          timeBudgetAcc + race.timeBudget.toString,
          distanceToBeatAcc + race.distanceToBeat.toString,
        )
    }

    Race(timeBudgetStr.toLong, distanceToBeatStr.toLong) :: Nil
  }

  val Real1: Input =
    Race(47, 282) :: Race(70, 1079) :: Race(75, 1147) :: Race(66, 1062) :: Nil

  val Real2: Input = convertInput(Real1)

  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${solve(Real1)}")
    println(s"Part 2: ${solve(Real2)}")
  }
}
