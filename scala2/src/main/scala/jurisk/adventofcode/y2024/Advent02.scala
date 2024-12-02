package jurisk.adventofcode.y2024

import jurisk.utils.CollectionOps.{IntegralSeqOps, VectorOps}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent02 {
  type Input = List[Vector[Int]]

  def parse(input: String): Input =
    input.parseLines(_.extractIntVector)

  private def validDiffs(diffs: Seq[Int])(range: Range): Boolean =
    diffs forall range.contains

  private val ValidDiffRanges                    = List(1 to 3, -3 to -1)
  private def isSafe1(row: Vector[Int]): Boolean = {
    val diffs = row.differences
    ValidDiffRanges exists validDiffs(diffs)
  }

  def part1(data: Input): Int =
    data count isSafe1

  private def removingOne(row: Vector[Int]): Seq[Vector[Int]] =
    row.indices map row.removeAt

  private def isSafe2(row: Vector[Int]): Boolean =
    isSafe1(row) || (removingOne(row) exists isSafe1)

  def part2(data: Input): Int =
    data count isSafe2

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/02$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
