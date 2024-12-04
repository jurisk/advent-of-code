package jurisk.adventofcode.y2024

import cats.implicits._
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.AllDirections
import jurisk.geometry.Direction2D.NE
import jurisk.geometry.Direction2D.NW
import jurisk.geometry.Direction2D.SE
import jurisk.geometry.Direction2D.SW
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._

import scala.annotation.tailrec

object Advent04 {
  type Input = Field2D[Char]
  val X = 'X'
  val M = 'M'
  val A = 'A'
  val S = 'S'

  private val Xmas = List(X, M, A, S)

  def parse(input: String): Input =
    Field2D.parseCharField(input)

  def part1(data: Input): Int = {
    @tailrec
    def isValid(
      c: Coords2D,
      d: Direction2D,
      need: List[Char],
    ): Boolean =
      need match {
        case Nil    => true
        case h :: t =>
          if (data(c) contains h) {
            isValid(c + d, d, t)
          } else {
            false
          }
      }

    data.allCoords
      .map(c => AllDirections.count(direction => isValid(c, direction, Xmas)))
      .sum
  }

  def part1Old(data: Input): Int = {
    val rows = data.rows
    val cols = data.columns

    val diagonals = data.allCoords
      .flatMap { c =>
        List((c.x + c.y, '/', c), (c.x - c.y, '\\', c))
      }
      .groupBy { case (n, s, _) => (n, s) }
      .map { case (_, coords) =>
        coords.flatMap { case (_, _, c) => data.at(c) }
      }

    (for {
      what   <- List(rows, cols, diagonals)
      slice  <- what
      search <- List(Xmas, Xmas.reverse)
      regex   = search.mkString.r
      result  = regex.findAllIn(slice.mkString).length
    } yield result).sum
  }

  def part2(data: Input): Int =
    data.countCoords { c =>
      val corners = Set(Set(NE, SW), Set(NW, SE)).map(directions =>
        directions.flatMap(direction => data(c + direction))
      )
      data(c).contains(A) && corners === Set(Set(M, S))
    }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/04$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
