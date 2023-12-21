package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent21 {
  final case class Input(
    field: Field2D[Boolean],
    start: Coords2D,
  )

  def parse(input: String): Input = {
    val temp = Field2D.parseCharField(input)
    val start = temp.filterCoordsByValue(_ == 'S').singleResultUnsafe

    val field = temp.map {
      case '.' => false
      case 'S' => false
      case '#' => true
    }

    Input(
      field,
      start,
    )
  }

  def part1(data: Input, steps: Int): Int = {
    val positions = Set(data.start)
    val results = Simulation.runNIterations(positions, steps) { case (current, counter) =>
      current flatMap { c =>
        data.field.adjacent4Where(c, _ == false)
      }
    }
    results.size
  }

  def part2(data: Input): Int =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/21$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 64)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
