package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp

object Advent01 extends SingleLineAdventApp[Int, Int]:
  val year: Int = 2020
  val exercise: Int = 1

  def solve(testCases: List[Int], n: Int): Int =
    testCases
      .combinations(n)
      .find { _.sum == 2020 }
      .getOrElse(sys.error("Failed"))
      .product

  def solution1(testCases: List[Int]): Int = solve(testCases, 2)
  def solution2(testCases: List[Int]): Int = solve(testCases, 3)
  
  def parseLine(line: String): Either[ErrorMessage, Int] = 
    line.toIntOption.toRight(ErrorMessage("Failed to parse"))
