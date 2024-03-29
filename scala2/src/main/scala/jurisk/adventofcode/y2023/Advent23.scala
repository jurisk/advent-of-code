package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2023.Advent23.Square.Forest
import jurisk.adventofcode.y2023.Advent23.Square.Path
import jurisk.adventofcode.y2023.Advent23.Square.Slope
import jurisk.algorithms.Backtracker
import jurisk.algorithms.Backtracking
import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.immutable.BiMap
import jurisk.collections.immutable.BiMap.BiDirectionalArrowAssociation
import jurisk.collections.immutable.graph.Graph
import jurisk.collections.immutable.graph.Graph.Distance
import jurisk.collections.immutable.graph.Graph.VertexId
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

import scala.collection.immutable

object Advent23 {
  final case class Input(
    field: Field2D[Square],
    start: Coords2D,
    goal: Coords2D,
  )

  sealed trait Square extends Product with Serializable
  object Square {
    case object Forest                                     extends Square
    case object Path                                       extends Square
    final case class Slope(direction: CardinalDirection2D) extends Square
  }

  def parse(input: String): Input = {
    val field = parseField(input)

    val start = field.topRowCoords
      .find(c => field.at(c).contains(Path))
      .getOrElse("Start not found".fail)
    val goal  = field.bottomRowCoords
      .find(c => field.at(c).contains(Path))
      .getOrElse("Goal not found".fail)

    Input(
      field,
      start,
      goal,
    )
  }

  private def parseField(input: String): Field2D[Square] =
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
    def next(field: Field2D[Square]): List[State] = {
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

  // This is slower than it could be. Could try two other things (likely together):
  //  1) Use Backtracking to solve
  //  2) Convert to the `Graph` (this one will be directed) and use that to solve Part 1
  def solve1(data: Input): Int = {
    val startState = State(
      visited = Set(data.start),
      steps = 0,
      current = data.start,
    )

    val reachable = Bfs.bfsReachable[State](
      startState,
      _.next(data.field),
    )

    val best = reachable.filter(_.current == data.goal).maxBy(_.steps)

    best.steps
  }

  def part1(data: Input): Long = solve1(data)

  def part2UsingPart1(data: Input): Long = {
    val updated = convertPart1ToPart2(data)
    solve1(updated)
  }

  // This was slower than `solve2Backtracking` so remains just a unit test and a usage example for `Backtracker`
  private[y2023] def solve2BacktrackingUsingBacktracker(
    start: VertexId,
    graph: Graph[Coords2D],
    goal: VertexId,
  ): Long = {
    var best = 0L

    final case class BacktrackingState(
      current: VertexId,
      visited: immutable.BitSet,
      steps: Distance,
    )

    Backtracker.solve(new Backtracking[Graph[Coords2D], BacktrackingState] {
      override def root(p: Graph[Coords2D]): BacktrackingState               =
        BacktrackingState(start, immutable.BitSet.empty, 0)
      override def reject(p: Graph[Coords2D], c: BacktrackingState): Boolean =
        c.visited.contains(c.current)
      override def accept(p: Graph[Coords2D], c: BacktrackingState): Boolean =
        false
      override def extensions(
        p: Graph[Coords2D],
        c: BacktrackingState,
      ): Seq[BacktrackingState] =
        if (c.current == goal) {
          best = best max c.steps
          Nil
        } else {
          graph.outgoingEdges(c.current).map { case (to, distance) =>
            BacktrackingState(to, c.visited + c.current, c.steps + distance)
          }
        }
    })(graph)

    best
  }

  private[y2023] def solve2Backtracking(
    start: VertexId,
    graph: Graph[Coords2D],
    goal: VertexId,
  ): Distance = {
    var best = 0L

    // Not stack safe, but the depth is not deep
    def backtrack(
      current: VertexId,
      visited: immutable.BitSet,
      steps: Distance,
    ): Unit =
      if (visited.contains(current)) {
        // reject
      } else {
        if (current == goal) {
          if (steps > best) {
            best = steps
          }
        } else {
          graph.outgoingEdges(current) foreach { case (to, distance) =>
            backtrack(to, visited + current, steps + distance)
          }
        }
      }

    backtrack(start, immutable.BitSet.empty, 0)

    best
  }

  private[y2023] def convertPart1ToPart2(input: Input): Input =
    input.copy(field = input.field.map {
      case Square.Forest => Square.Forest
      case Square.Path   => Square.Path
      case Slope(_)      => Square.Path
    })

  private[y2023] def convertToSimplified(
    input: Input
  ): (VertexId, Graph[Coords2D], VertexId) = {
    val (start1, graph, goal1) = inputToGraph(input)

    val (start, simplified, goal) = {
      val simplified = graph.simplifyByPathContraction(Set(start1, goal1))
      (
        simplified.labelToVertex(input.start),
        simplified,
        simplified.labelToVertex(input.goal),
      )
    }

    (start, simplified, goal)
  }

  def part2(data: Input): Long = {
    val converted = convertPart1ToPart2(data)

    val (start, simplified, goal) = convertToSimplified(converted)

    println(
      s"Simplified to ${simplified.vertexCount} vertices, ${simplified.allEdges.size} edges"
    )
    val result = solve2Backtracking(start, simplified, goal)

    val debug = false
    if (debug)
      println(
        Graph.toDotDigraph[Coords2D](
          simplified,
          c => s"x${c.x}y${c.y}",
          Map(converted.start -> "green", converted.goal -> "blue"),
        )
      )

    result
  }

  private[y2023] def inputToGraph(
    input: Input
  ): (VertexId, Graph[Coords2D], VertexId) = {
    val graph = Field2D.toGraphCardinalDirections(input.field) {
      case ((fromC, fromV), d, (toC, toV)) =>
        Set(fromV, toV).forall(_ == Path).option(1L)
    }

    (graph.labelToVertex(input.start), graph, graph.labelToVertex(input.goal))
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
