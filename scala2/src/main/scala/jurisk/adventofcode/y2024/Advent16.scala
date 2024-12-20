package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import jurisk.algorithms.pathfinding.Bfs
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.collections.immutable.ImmutableBitSet
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D
import jurisk.geometry.Field2D.coordsToInt
import jurisk.geometry.Rotation
import jurisk.utils.CollectionOps.OptionOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.ToInt

object Advent16 {
  type Input        = (State, Field2D[Boolean], Coords2D)
  private type Cost = Int

  final case class State(position: Coords2D, direction: CardinalDirection2D) {
    def successors(field: Field2D[Boolean]): List[(State, Cost)] = {
      val next     = position + direction
      val straight = field.at(next) match {
        case Some(false) => State(next, direction).some
        case _           => none
      }
      val rotateL  = copy(direction = direction.rotate(Rotation.Left90))
      val rotateR  = copy(direction = direction.rotate(Rotation.Right90))
      (rotateL, 1000) :: (rotateR, 1000) :: straight.map((_, 1)).toList
    }
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
    (State(start, Direction2D.E), field, end)
  }

  def part1(data: Input): Cost = {
    val (state, field, end) = data

    val (_, result) = Dijkstra
      .dijkstra[State, Int](
        state,
        state => state.successors(field),
        _.position == end,
      )
      .orFail("Path not found")

    result
  }

  def part2(data: Input): Cost = {
    val best = part1(data)
    println(s"Best: $best")

    val (state, field, end) = data

    implicit val c2i: ToInt[Coords2D] = coordsToInt(field)

    var bestCostSoFar = Map.empty[State, Cost]
    var visitedCoords = ImmutableBitSet.empty[Coords2D]

    // Note: Actually, `bfsVisitAll` keeps another "visited" set, which is a duplicate
    Bfs.bfsVisitAll[(State, Cost, ImmutableBitSet[Coords2D])](
      (state, 0, ImmutableBitSet.empty),
      { case (state, cost, path) =>
        if (cost < best) {
          if (cost > bestCostSoFar.getOrElse(state, Int.MaxValue)) {
            Nil
          } else {
            bestCostSoFar += (state -> cost)
            state
              .successors(field)
              .map { case (n, c) =>
                (n, cost + c, path + n.position)
              }
          }
        } else {
          Nil
        }
      },
      { case (state, cost, path) =>
        if ((state.position == end) && (cost == best)) {
          visitedCoords = visitedCoords ++ path
          println(s"End state found: ${visitedCoords.size}")
        }
      },
    )

    visitedCoords.size
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/16$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
