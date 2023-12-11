package jurisk.adventofcode.y2023

import jurisk.geometry.{Field2D, SparseBooleanField}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.mutable

final case class BigCoords2D(x: BigInt, y: BigInt) {
  def +(other: BigCoords2D): BigCoords2D =
    BigCoords2D(x + other.x, y + other.y)

  def -(other: BigCoords2D): BigCoords2D =
    BigCoords2D(x - other.x, y - other.y)

  def manhattanDistanceToOrigin: BigInt =
    x.abs + y.abs

  def manhattanDistance(other: BigCoords2D): BigInt =
    (this - other).manhattanDistanceToOrigin

}

object Advent11 {
  type Input = Set[BigCoords2D]

  def parse(input: String): Input = {
    val field = Field2D.parseFromString(
      input,
      {
        case '#' => true
        case '.' => false
        case ch  => ch.toString.fail
      },
    )

    val coords = field.filterCoordsByValue(_ == true).toSet
    coords.map { c =>
      BigCoords2D(c.x, c.y)
    }
  }

  def expandColumns(data: Set[BigCoords2D], exp: Int): Set[BigCoords2D] = {
    val sorted                            = data.toList.sortBy(_.x)
    val results: mutable.Set[BigCoords2D] = mutable.Set.empty

    var currX         = sorted.head.x
    var diffX: BigInt = 0
    sorted foreach { point =>
      if (point.x > currX) {
        val diff = point.x - currX
        if (diff > 1) {
          diffX += (diff - 1) * (exp - 1)
        }
        currX = point.x
      }

      results.add(point + BigCoords2D(diffX, 0))
    }

    results.toSet
  }

  def expandRows(data: Set[BigCoords2D], exp: BigInt): Set[BigCoords2D] = {
    val sorted                            = data.toList.sortBy(_.y)
    val results: mutable.Set[BigCoords2D] = mutable.Set.empty

    var currY         = sorted.head.y
    var diffY: BigInt = 0
    sorted foreach { point =>
      if (point.y > currY) {
        val diff = point.y - currY
        if (diff > 1) {
          diffY += (diff - 1) * (exp - 1)
        }
        currY = point.y
      }

      results.add(point + BigCoords2D(BigInt(0), diffY))
    }

    results.toSet
  }
//
//  def print(data: Input): Unit = {
//    println(SparseBooleanField(data, 1000).toDebugRepresentation)
//  }

  /*
    Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no galaxies should all actually be twice as big.
   */
  def expand(data: Input, exp: Int): Set[BigCoords2D] = {
//    print(data)
    val result = expandColumns(expandRows(data, exp), exp)
//    print(result)
    result
  }

  def solve(data: Input, exp: Int): BigInt = {
    val expanded = expand(data, exp)

    (for {
      a <- expanded.toList
      b <- expanded.toList
    } yield a.manhattanDistance(b)).sum / 2
  }

  def part1(data: Input): BigInt =
    solve(data, 2)

  def part2(data: Input): BigInt =
    solve(data, 1000000)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/11.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
