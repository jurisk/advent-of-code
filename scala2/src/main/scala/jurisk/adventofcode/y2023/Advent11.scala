package jurisk.adventofcode.y2023

import jurisk.geometry.{Coords2D, Field2D, SparseBooleanField}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.mutable

object Advent11 {
  type Input = Set[Coords2D]

  def parse(input: String): Input = {
    val field = Field2D.parseFromString(input, {
      case '#' => true
      case '.' => false
      case ch => ch.toString.fail
    })

    val coords = field.filterCoordsByValue(_ == true).toSet
    coords
  }

  def expandColumns(data: Set[Coords2D], exp: Int): Set[Coords2D] = {
    val sorted = data.toList.sortBy(_.x)
    val results: mutable.Set[Coords2D] = mutable.Set.empty

    var currX = sorted.head.x
    var diffX = 0
    sorted foreach { point =>
      if (point.x > currX) {
        val diff = point.x - currX
        if (diff > 1) {
          diffX += (diff - 1) * exp
        }
        currX = point.x
      }

      results.add(point + Coords2D(diffX, 0))
    }

    results.toSet
  }

  def expandRows(data: Set[Coords2D], exp: Int): Set[Coords2D] = {
    val sorted = data.toList.sortBy(_.y)
    val results: mutable.Set[Coords2D] = mutable.Set.empty

    var currY = sorted.head.y
    var diffY = 0
    sorted foreach { point =>
      if (point.y > currY) {
        val diff = point.y - currY
        if (diff > 1) {
          diffY += (diff - 1) * exp
        }
        currY = point.y
      }

      results.add(point + Coords2D(0, diffY))
    }

    results.toSet
  }

  def print(data: Input): Unit = {
    println(SparseBooleanField(data, 1000).toDebugRepresentation)
  }

  /*
    Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no galaxies should all actually be twice as big.
   */
  def expand(data: Input, exp: Int): Set[Coords2D] = {
//    print(data)
    val result = expandColumns(expandRows(data, exp), exp)
//    print(result)
    result
  }

  def solve(data: Input, exp: Int): Int = {
    val expanded = expand(data, exp)

    (for {
      a <- expanded.toList
      b <- expanded.toList
    } yield a.manhattanDistance(b)).sum / 2
  }

  def part1(data: Input): Int =
    solve(data, 1)

  def part2(data: Input): Int =
    solve(data, 1000000)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/11.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
