package jurisk.adventofcode.y2024

import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.collections.mutable.MutableBitSet
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D.coordsToInt
import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.ToInt
import jurisk.utils.conversions.syntax.ToIntOps

import scala.collection.immutable.BitSet

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

    println(s"Result: $result")
    result
  }

  def part2(data: Input): N = {
    val best = part1(data)
    println(s"Best: $best")

    val (state, field, end) = data

    implicit val c2i: ToInt[Coords2D] = coordsToInt(field)

    var visited2 = Map.empty[State, Int]
    var visited = Set.empty[(State, BitSet)]
    var mega    = BitSet.empty

    Bfs.bfsVisitAll[(State, N, BitSet)](
      (state, 0, BitSet.empty),
      { case (state, cost, path) =>
        if (cost < best) {
          if (cost > visited2.getOrElse(state, Int.MaxValue)) {
            Nil
          } else {
            visited2 += (state -> cost)
            if (!visited.contains((state, path))) {
              visited += ((state, path))
              if (visited.size % 100_000 == 0) {
                println(s"Visited: ${visited.size}, cost = $cost, best = $best")
              }

              state
                .successors(field)
                .map(n => (n._1, cost + n._2, path + n._1.position.toInt))
            } else {
              Nil
            }
          }
        } else {
          Nil
        }
      },
      { case (state, cost, path) =>
//        println(s"State: $state, End: $end, Cost: $cost, Path: $path")
        if ((state.position == end) && (cost == best)) {
          mega = mega ++ path
          println(s"End state found: ${mega.size}")
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
