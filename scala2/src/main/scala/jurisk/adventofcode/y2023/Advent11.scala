package jurisk.adventofcode.y2023

import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq
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
  type Galaxies = ArraySeq[BigCoords2D]

  def parse(input: String): Galaxies = {
    val field = Field2D.parseFromString(
      input,
      {
        case '#' => true
        case '.' => false
        case ch  => ch.toString.fail
      },
    )

    val coords = field.filterCoordsByValue(_ == true)

    val results = coords.map { c =>
      BigCoords2D(c.x, c.y)
    }

    ArraySeq.from(results)
  }

  def expandColumns(data: Galaxies, factor: BigInt): Galaxies = {
    val sorted                                    = data.toList.sortBy(_.x)
    val results: mutable.ArrayBuffer[BigCoords2D] = mutable.ArrayBuffer.empty

    var currX         = sorted.head.x
    var diffX: BigInt = 0
    sorted foreach { point =>
      if (point.x > currX) {
        val diff = point.x - currX
        if (diff > 1) {
          diffX += (diff - 1) * (factor - 1)
        }
        currX = point.x
      }

      results += point + BigCoords2D(diffX, 0)
    }

    ArraySeq.from(results)
  }

  def expandRows(data: Galaxies, factor: BigInt): Galaxies = {
    val sorted                                    = data.toList.sortBy(_.y)
    val results: mutable.ArrayBuffer[BigCoords2D] = mutable.ArrayBuffer.empty

    var currY         = sorted.head.y
    var diffY: BigInt = 0
    sorted foreach { point =>
      if (point.y > currY) {
        val diff = point.y - currY
        if (diff > 1) {
          diffY += (diff - 1) * (factor - 1)
        }
        currY = point.y
      }

      results += (point + BigCoords2D(BigInt(0), diffY))
    }

    ArraySeq.from(results)
  }

//  def print(data: Input): Unit = {
//    println(SparseBooleanField(data, 1000).toDebugRepresentation)
//  }

  /*
    Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no galaxies should all actually be twice as big.
   */
  def expand(data: Galaxies, factor: BigInt): Galaxies = {
//    print(data)
    val result = expandColumns(expandRows(data, factor), factor)
//    print(result)
    result
  }

  def solve(data: Galaxies, factor: BigInt): BigInt = {
    val expanded = expand(data, factor)

    (for {
      a <- expanded.toList
      b <- expanded.toList
    } yield a.manhattanDistance(b)).sum / 2
  }

  def part1(data: Galaxies): BigInt =
    solve(data, 2)

  def part2(data: Galaxies): BigInt =
    solve(data, 1_000_000)

  def parseFile(fileName: String): Galaxies =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Galaxies = parseFile("2023/11.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
