package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp

object Advent03 extends SingleLineAdventApp[String, Long]:
  override val year: Int = 2020
  override val exercise: Int = 3

  private val Tree = '#'

  override def parseLine(line: String): Either[ErrorMessage, String] = line.asRight[ErrorMessage]

  private def solve(data: List[String], pairs: List[(Int, Int)]) = {
    def f(jumpRight: Int, jumpDown: Int): Long =
      (data.indices by jumpDown)
        .zipWithIndex
        .tail
        .count { case (r, idx) =>
          val row = data(r)
          row((jumpRight * idx) % row.length) == Tree
        }

    pairs.map((jumpRight, jumpDown) => f(jumpRight, jumpDown)).product
  }

  override def solution1(input: List[String]): Long = solve(input, List((3, 1)))

  override def solution2(input: List[String]): Long = solve(input, List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)))
