package jurisk.adventofcode.y2024

import jurisk.algorithms.pathfinding.{AStar, Bfs}
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent20 {
  type Input = (State, Field2D[Boolean], Coords2D)
  type N     = Long

  final case class Cheat(from: Coords2D, to: Coords2D)

  final case class State(position: Coords2D, cheatUsed: Option[Cheat] = None) {
    def successors(field: Field2D[Boolean]): List[(State, Int)] =
      field
        .neighboursFor(position, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          copy(position = n)
        }
        .map { r =>
          (r, 1)
        }
  }

  final case class State2(
    position: Coords2D,
    cheatUsed: Option[Cheat] = None,
    cost: Int = 0,
  ) {
    def successors(field: Field2D[Boolean]): List[State2] = {
      val normal = field
        .neighboursFor(position, includeDiagonal = false)
        .filter(field.at(_).contains(false))
        .map { n =>
          copy(position = n, cost = cost + 1)
        }

      val cheatBased = cheatUsed match {
        case None    =>
          Direction2D.CardinalDirections
            .map { d =>
              position + d.diff * 2
            }
            .filter(field.at(_).contains(false))
            .map { n =>
              copy(
                position = n,
                cheatUsed = Some(Cheat(position, n)),
                cost = cost + 2,
              )
            }
        case Some(_) => Nil
      }

      cheatBased ::: normal
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

  def part1(data: Input, saveAtLeast: Int): N = {
    val (state1, field, end) = data
    val (_, cost)            = AStar
      .aStar[State, Int](
        state1,
        state => state.successors(field),
        state => state.position.manhattanDistance(end),
        _.position == end,
      )
      .getOrElse("No path found".fail)
    val goalCostThreshold    = cost - saveAtLeast
    println(s"Without cheats = $cost, goal cost threshold = $goalCostThreshold")

    val state = State2(state1.position)

    var validCheats = Set.empty[Cheat]

    def succ(state: State2): List[State2] =
      state
        .successors(field)
        .filter(_.cost <= goalCostThreshold)

    def visit(state: State2): Unit =
      if (state.position == end && state.cost <= goalCostThreshold) {
        state.cheatUsed.foreach(validCheats += _)
      }

    Bfs.bfsVisitAll[State2](
      state,
      succ,
      visit,
    )

    validCheats.size
  }

  def part2(data: Input): N =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/20$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 100)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
