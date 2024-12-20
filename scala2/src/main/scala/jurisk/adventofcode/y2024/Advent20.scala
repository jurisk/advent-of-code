package jurisk.adventofcode.y2024

import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent20 {
  type Input = (Coords2D, Field2D[Boolean], Coords2D)
  type N     = Long

  final case class Cheat(from: Coords2D, to: Coords2D) {
    def distance: Int = from manhattanDistance to
  }

  def parse(input: String): Input = {
    val charField = Field2D.parseCharField(input)
    val start     =
      charField.findCoordsByValue('S').getOrElse("Start not found".fail)
    val end       = charField.findCoordsByValue('E').getOrElse("End not found".fail)
    val field     = charField.mapByCoordsWithValues { case (_, c) =>
      c match {
        case 'S' => false
        case 'E' => false
        case '#' => true
        case '.' => false
        case _   => s"Unknown character $c in field".fail
      }
    }
    (start, field, end)
  }

  def solve(data: Input, saveAtLeast: Int, maxCheat: Int): N = {
    val (start, field, end) = data

    def successors(c: Coords2D): List[(Coords2D, Int)] =
      field
        .neighboursFor(c, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          (n, 1)
        }

    val fromStart = Dijkstra.dijkstraAll[Coords2D, Int](
      start,
      successors,
    )

    val fromEnd = Dijkstra.dijkstraAll[Coords2D, Int](
      end,
      successors,
    )

    val (_, cost)         = fromStart.getOrElse(end, "No path from start to end".fail)
    val goalCostThreshold = cost - saveAtLeast

    println(s"Without cheats = $cost, goal cost threshold = $goalCostThreshold")

    def validCheat(cheat: Cheat): Boolean =
      (fromStart.get(cheat.from), fromEnd.get(cheat.to)) match {
        case (Some((_, startCost)), Some((_, endCost))) =>
          startCost + endCost + cheat.distance <= goalCostThreshold
        case _                                          =>
          false
      }

    val empties = field.allCoords
      .filter(field.at(_).contains(false))
    val cheats  =
      for {
        // Could be optimised by avoiding processing all O(N^2), as if we know one coordinate, we can generate all others within maxCheat distance more efficiently
        c1 <- empties
        c2 <- empties
        if c1.manhattanDistance(c2) <= maxCheat
        if c1 != c2
      } yield Cheat(c1, c2)

    println(s"Cheats: ${cheats.size}")

    val selectedCheats =
      cheats
        .filter(validCheat)
        .toSet

    println(s"Selected cheats: ${selectedCheats.size}")
    selectedCheats.foreach(println)

    selectedCheats.size
  }

  def part1(data: Input, saveAtLeast: Int): N =
    solve(data, saveAtLeast, 2)

  def part2(data: Input, saveAtLeast: Int): N =
    solve(data, saveAtLeast, 20)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/20$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 100)}")
    println(s"Part 2: ${part2(realData, 100)}")
  }
}
