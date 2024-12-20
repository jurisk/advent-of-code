package jurisk.adventofcode.y2024

import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent20 {
  type Input = (State, Field2D[Boolean], Coords2D)
  type N     = Long

  final case class Cheat(from: Coords2D, to: Coords2D) {
    def distance: Int = from manhattanDistance to
  }

  final case class State(position: Coords2D, cheatUsed: Option[Cheat] = None) {
    def successors(field: Field2D[Boolean]): List[(State, Int)] =
      field
        .neighboursFor(position, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          (copy(position = n), 1)
        }
  }

  def parse(input: String): Input = {
    val charField = Field2D.parseCharField(input)
    val start     = charField.findCoordsByValue('S').get
    val end       = charField.findCoordsByValue('E').get
    val field     = charField.mapByCoordsWithValues { case (_, c) =>
      c match {
        case 'S' => false
        case 'E' => false
        case '#' => true
        case '.' => false
        case _   => s"Unknown character $c in field".fail
      }
    }
    (State(start), field, end)
  }

  def solve(data: Input, saveAtLeast: Int, maxCheat: Int): N = {
    val (state1, field, end) = data

    val fromStart = Dijkstra.dijkstraAll[Coords2D, Int](
      state1.position,
      field
        .neighboursFor(_, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          (n, 1)
        },
      returnStart = true,
    )

    val fromEnd = Dijkstra.dijkstraAll[Coords2D, Int](
      end,
      field
        .neighboursFor(_, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          (n, 1)
        },
      returnStart = true,
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
