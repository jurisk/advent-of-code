package jurisk.adventofcode.y2024

import jurisk.algorithms.Backtracker
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra, Pathfinding}
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import java.util

object Advent16 {
  type Input = (State, Field2D[Boolean], Coords2D)
  type N     = Int

  final case class State(position: Coords2D, direction: CardinalDirection2D) {
    def successors(field: Field2D[Boolean]): List[(State, N)] = {
      val next     = position + direction
      val straight = field.at(next) match {
        case Some(false) => Option(State(next, direction))
        case _           => None
      }
      val rotateL  = copy(direction = direction.rotate(Rotation.Left90))
      val rotateR  = copy(direction = direction.rotate(Rotation.Right90))
      (rotateL, 1000) :: (rotateR, 1000) :: straight.map((_, 1)).toList
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
    (State(start, Direction2D.E), field, end)
  }

  def part1(data: Input): N = {
    val (state, field, end) = data

//    println(s"State: $state, End: $end")
    Field2D.printBooleanField(field)

    val (_, result) = Dijkstra
      .dijkstra[State, Int](
        state,
        state => state.successors(field),
        _.position == end,
      )
      .get
    result
  }
//
//  private var megaVisited = Set.empty[(State, Set[Coords2D])]
//  def f(state: State, field: Field2D[Boolean], end: Coords2D, best: Int, costSoFar: Int, visited: Set[Coords2D]): Set[Coords2D] = {
//    println(s"State: $state, End: $end, Cost: $costSoFar, Visited: $visited, Megavisited: $megaVisited")
//    megaVisited += ((state, visited))
//    if (costSoFar > best) {
//      Set.empty
//    } else {
//      if (state.position == end) {
//        visited
//      } else {
//          state.successors(field).flatMap { case (n, cost) =>
//            val newVisited = visited + n.position
//            if (!megaVisited.contains((n, newVisited))) {
//              f(n, field, end, best, costSoFar + cost, newVisited)
//            } else {
//              Set.empty
//            }
//          }.toSet
//      }
//    }
//  }

  def part2(data: Input): N = {
    val best = part1(data)
    println(s"Best: $best")

    val (state, field, end) = data

    var visited = Set.empty[(State, Set[Coords2D])]
    var mega    = Set.empty[Coords2D]

    Bfs.bfsVisitAll[(State, N, List[Coords2D])](
      (state, 0, Nil),
      { case (state, cost, path) =>
        if (cost < best) {
          if (!visited.contains((state, path.toSet))) {
            visited += ((state, path.toSet))

            state
              .successors(field)
              .map(n => (n._1, cost + n._2, n._1.position :: path))
          } else {
            Nil
          }
        } else {
          Nil
        }
      },
      { case (state, cost, path) =>
//        println(s"State: $state, End: $end, Cost: $cost, Path: $path")
        if ((state.position == end) && (cost == best)) {
          mega = mega ++ path.toSet
        }
      },
    )

    mega.size
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
