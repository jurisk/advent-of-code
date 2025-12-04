package jurisk.adventofcode.y2025

import cats.implicits._
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Simulation

object Advent04 {
  type Input = Field2D[Boolean]
  type N     = Int

  def parse(input: String): Input =
    Field2D.parseBooleanField(input, trueChar = '@')

  private def removable(data: Input): Seq[Coords2D] =
    data.allCoords.filter { c =>
      data.at(c) == true.some && data.adjacent8Values(c).count(_ == true) < 4
    }

  def part1(data: Input): N =
    removable(data).size

  def part2(data: Input): N =
    Simulation.run((data, 0)) { case (state, count) =>
      val removed = removable(state)
      if (removed.isEmpty) count.asLeft
      else {
        val newState = removed.foldLeft(state) { case (s, c) =>
          s.updatedAtUnsafe(c, false)
        }
        (newState, count + removed.size).asRight
      }
    }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/04$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
