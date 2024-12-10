package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.geometry.Coordinates2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import monocle.Lens
import monocle.macros.GenLens

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Advent11 {
  private type BigIntCoords2D = Coordinates2D[BigInt]
  private type Galaxies       = ArraySeq[BigIntCoords2D]

  def parse(input: String): Galaxies = {
    val field = Field2D.parseBooleanField(input)

    val coords = field.filterCoordsByValue(true)

    val results = coords.map { c =>
      Coordinates2D[BigInt](c.x, c.y)
    }

    ArraySeq.from(results)
  }

  private def expandGeneric(
    data: Galaxies,
    factor: BigInt,
    access: Lens[BigIntCoords2D, BigInt],
  ): Galaxies = {
    val sorted = data.toList.sortBy(access.get)

    sorted.headOption match {
      case Some(first) =>
        val results: mutable.ArrayBuffer[BigIntCoords2D] =
          mutable.ArrayBuffer.empty

        var previous                = access.get(first)
        var accumulatedDiff: BigInt = 0
        sorted foreach { point =>
          val pointCoordinate = access.get(point)
          val jump            = (pointCoordinate - previous - 1) max 0
          accumulatedDiff += jump * (factor - 1)

          val newPoint = access.modify(_ + accumulatedDiff)(point)

          results += newPoint
          previous = pointCoordinate
        }

        ArraySeq.from(results)
      case None        =>
        ArraySeq.empty
    }
  }

  /*
   * Due to something involving gravitational effects, only some space expands.
   * In fact, the result is that any rows or columns that contain no galaxies should all actually be `factor` times as
   * big.
   */
  private[y2023] def expand(data: Galaxies, factor: BigInt): Galaxies = {
    def expandColumns(data: Galaxies, factor: BigInt): Galaxies =
      expandGeneric(data, factor, GenLens[BigIntCoords2D](_.x))

    def expandRows(data: Galaxies, factor: BigInt): Galaxies =
      expandGeneric(data, factor, GenLens[BigIntCoords2D](_.y))

    expandColumns(expandRows(data, factor), factor)
  }

  def solve(data: Galaxies, factor: BigInt): BigInt = {
    val expanded = expand(data, factor)

    expanded
      .combinations(2)
      .map {
        case ArraySeq(a, b) => a.manhattanDistance(b)
        case unexpected     => unexpected.toString.fail
      }
      .sum
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
