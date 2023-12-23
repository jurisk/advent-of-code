package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2023.Advent23.Square.{Forest, Path, Slope}
import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent23 {
  type Input = Field2D[Square]

  sealed trait Square extends Product with Serializable
  object Square {
    case object Forest extends Square
    case object Path extends Square
    final case class Slope(direction: CardinalDirection2D) extends Square
  }

  def parse(input: String): Input =
    Field2D.parseFromBiMap(input, BiMap(
      '#' <-> Forest,
      '.' <-> Path,
      '>' <-> Slope(Direction2D.E),
      'v' <-> Slope(Direction2D.S),
      '^' <-> Slope(Direction2D.N),
      '<' <-> Slope(Direction2D.W),
    ))

  final case class State(
    visited: Set[Coords2D],
    steps: Int,
    current: Coords2D,
  ) {
    def next(field: Input): List[State] = {
      val candidates = field.atOrElse(current, Forest) match {
        case Square.Forest => "wtf".fail
        case Square.Path => field.adjacent4(current)
        case Slope(direction) => {
          val n = current + direction
          if (field.isValidCoordinate(n))
            n :: Nil
          else
            Nil
        }
      }

      val valid = candidates.filter(c => field.atOrElse(c, Forest) != Forest).filterNot(visited.contains)

      valid map { c =>
        State(
          visited = visited + c,
          steps = steps + 1,
          current = c
        )
      }
    }
  }

  def solve1(data: Input): Int = {
    val startCoords = data.topRowCoords.find(c => data.at(c).contains(Path)).get
    val goalCoords = data.bottomRowCoords.find(c => data.at(c).contains(Path)).get

    val startState = State(
      visited = Set(startCoords),
      steps = 0,
      current = startCoords,
    )

    val reachable = Bfs.bfsReachable[State](
      startState,
      _.next(data),
    )

    reachable.filter(_.current == goalCoords).maxBy(_.steps).steps
  }

  def part1(data: Input): Long = solve1(data)

  def part2(data: Input): Long = {
    val updated = data.map {
      case Square.Forest => Square.Forest
      case Square.Path => Square.Path
      case Slope(_) => Square.Path
    }

    solve1(updated)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/23$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
