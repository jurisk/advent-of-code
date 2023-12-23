package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2023.Advent23.Square.Forest
import jurisk.adventofcode.y2023.Advent23.Square.Path
import jurisk.adventofcode.y2023.Advent23.Square.Slope
import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.collections.SetOfTwo
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D
import jurisk.graph.Edge
import jurisk.graph.UndirectedGraph
import jurisk.graph.UndirectedGraph.VertexId
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.mutable

object Advent23 {
  type Input = Field2D[Square]

  sealed trait Square extends Product with Serializable
  object Square {
    case object Forest                                     extends Square
    case object Path                                       extends Square
    final case class Slope(direction: CardinalDirection2D) extends Square
  }

  def parse(input: String): Input =
    Field2D.parseFromBiMap(
      input,
      BiMap(
        '#' <-> Forest,
        '.' <-> Path,
        '>' <-> Slope(Direction2D.E),
        'v' <-> Slope(Direction2D.S),
        '^' <-> Slope(Direction2D.N),
        '<' <-> Slope(Direction2D.W),
      ),
    )

  final case class State(
    visited: Set[Coords2D],
    steps: Int,
    current: Coords2D,
  ) {
    def next(field: Input): List[State] = {
      val candidates = field.atOrElse(current, Forest) match {
        case Square.Forest    => "Current location should not be forest".fail
        case Square.Path      => field.adjacent4(current)
        case Slope(direction) =>
          val n = current + direction
          if (field.isValidCoordinate(n))
            n :: Nil
          else
            Nil
      }

      val valid = candidates
        .filter(c => field.atOrElse(c, Forest) != Forest)
        .filterNot(visited.contains)

      valid map { c =>
        State(
          visited = visited + c,
          steps = steps + 1,
          current = c,
        )
      }
    }
  }

  def solve1(data: Input): Int = {
    val startCoords = data.topRowCoords.find(c => data.at(c).contains(Path)).get
    val goalCoords  =
      data.bottomRowCoords.find(c => data.at(c).contains(Path)).get

    val startState = State(
      visited = Set(startCoords),
      steps = 0,
      current = startCoords,
    )

    val reachable = Bfs.bfsReachable[State](
      startState,
      _.next(data),
    )

    val best = reachable.filter(_.current == goalCoords).maxBy(_.steps)

   best.steps
  }

  def part1(data: Input): Long = solve1(data)

  def part2UsingPart1(data: Input): Long = {
    val updated = convertPart1ToPart2(data)
    solve1(updated)
  }

  def solve2Backtracking(
    graph: UndirectedGraph[Coords2D],
    start: VertexId,
    goal: VertexId,
  ): Long = {
    var best    = 0L
    val visited = mutable.BitSet.fromSpecific(start :: Nil)

    // TODO: Can you make the backtracking logic more generic and extract it out?
    // TODO: Make it stack safe?
    // Not stack safe, but the search tree is not that deep
    def backtrack(current: VertexId, steps: Long): Unit =
      if (current == goal) {
        if (steps > best) {
          best = steps
          println(s"Found better $best")
        }
      } else {
        graph.edgesFor(current) foreach { candidate =>
          val to = candidate.other(current)
          if (!visited.contains(to)) {
            visited.add(to)
            backtrack(to, steps + candidate.distance)
            visited.remove(to)
          }
        }

      }

    backtrack(start, 0)

    best
  }

  // TODO:  Move out - and I think the UndirectedGraph constructor should just get a Seq[(SetOfTwo[L], distance)],
  //        so this could even be split into two parts.
  def fieldToGraph(field: Input): UndirectedGraph[Coords2D] = {
    var nextIndex                                = 0
    var labelToIndexMap: Map[Coords2D, VertexId] = Map.empty
    field.allCoords.foreach { c =>
      field.at(c).foreach {
        case Square.Forest =>
        case _             =>
          labelToIndexMap = labelToIndexMap + (c -> nextIndex)
          nextIndex += 1
      }
    }

    var edges: Set[Edge] = Set.empty

    field.allConnectionsDirectional foreach { case (a, _, b) =>
      if (field.at(a).contains(Path) && field.at(b).contains(Path)) {
        val vertices = SetOfTwo(labelToIndexMap(a), labelToIndexMap(b))
        edges += Edge(vertices, 1)
      }
    }

    UndirectedGraph(BiMap.from(labelToIndexMap), edges)
  }

  private def convertPart1ToPart2(data: Input): Input =
    data.map {
      case Square.Forest => Square.Forest
      case Square.Path   => Square.Path
      case Slope(_)      => Square.Path
    }

  def part2(data: Input): Long = {
    val field = convertPart1ToPart2(data)

    val startCoords =
      field.topRowCoords.find(c => field.at(c).contains(Path)).getOrElse("Start not found".fail)
    val goalCoords  =
      field.bottomRowCoords.find(c => field.at(c).contains(Path)).getOrElse("Goal not found".fail)

    val graph = fieldToGraph(field)
    val start = graph.labelToVertex(startCoords)
    val goal = graph.labelToVertex(goalCoords)

    val simplified = graph.simplify(Set(start, goal))
    println(UndirectedGraph.toDot(simplified, start, goal))
    solve2Backtracking(simplified, start, goal)
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
