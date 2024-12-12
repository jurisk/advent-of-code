package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxFoldableOps0
import jurisk.algorithms.pathfinding.ConnectedComponents
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent12 {
  type Input = Field2D[Char]
  type N     = Int

  def parse(input: String): Input =
    Field2D.parseCharField(input)

  private def perimeter(coords: Set[Coords2D]): N =
    coords.toList.map {
      _.neighbours(includeDiagonal = false).filterNot(coords.contains).size
    }.sum

  private def ascendingSequences(data: List[Int]): N = data match {
    case Nil      => 0
    case _ :: Nil => 1
    case longer   =>
      longer.sliding2.count { case (a, b) =>
        a + 1 != b
      } + 1
  }

  private def sides(
    direction: Direction2D,
    groupBy: Coords2D => Int,
    evaluate: Coords2D => Int,
    coords: Set[Coords2D],
  ): N =
    coords.toList
      .filterNot(c => coords.contains(c + direction))
      .groupBy(groupBy)
      .map { case (_, cs) =>
        val a = cs.map(evaluate).sorted
        ascendingSequences(a)
      }
      .sum

  private def numberOfSides(coords: Set[Coords2D]): N =
    List[(Direction2D, Coords2D => Int, Coords2D => Int)](
      (Direction2D.N, _.y, _.x),
      (Direction2D.E, _.x, _.y),
      (Direction2D.S, _.y, _.x),
      (Direction2D.W, _.x, _.y),
    ).map { case (direction, groupBy, evaluate) =>
      sides(direction, groupBy, evaluate, coords)
    }.sum

  private def solve(f: Set[Coords2D] => N)(data: Input): N = {
    val regions = ConnectedComponents.connectedComponents[Coords2D](
      data.allCoords,
      c =>
        data
          .neighboursFor(c, includeDiagonal = false)
          .filter(data.at(_) == data.at(c)),
    )

    regions.toList.map { coords =>
      coords.size * f(coords)
    }.sum
  }

  private[y2024] val part1 = solve(perimeter)(_)
  private[y2024] val part2 = solve(numberOfSides)(_)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/12$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
