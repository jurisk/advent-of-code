package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2023.Advent23.Square.{Forest, Path, Slope}
import jurisk.algorithms.pathfinding.Bfs
import jurisk.algorithms.{Backtracker, Backtracking}
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.graph.Graph
import jurisk.graph.Graph.{Distance, VertexId}
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
      .getOrElse(s"Start not found".fail)
    val goal  = field.bottomRowCoords
      .find(c => field.at(c).contains(Path))
      .getOrElse(s"Goal not found".fail)

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

  // TODO:  Try two more things (likely together):
  //        1) Use Backtracking to solve
  //        2) Convert to the `Graph` (this one will be directed) and use that to solve Part 1
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
    graph: Graph[Coords2D],
    start: VertexId,
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
          graph.outgoingEdges(c.current).toSeq.map { case (to, distance) =>
            BacktrackingState(to, c.visited + c.current, c.steps + distance)
          }
        }
    })(graph)

    best
  }

  private[y2023] def solve2Backtracking(
    graph: Graph[Coords2D],
    start: VertexId,
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
    val graph = fieldToGraph(input.field)

    val (start, simplified, goal) = {
      val start      = graph.labelToVertex(input.start)
      val goal       = graph.labelToVertex(input.goal)
      val simplified = graph.simplify(Set(start, goal))
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

    val result = solve2Backtracking(simplified, start, goal)

    println(Graph.toDotDigraph(simplified, converted.start, converted.goal))

    result
  }

  private def fieldToGraph(field: Field2D[Square]): Graph[Coords2D] =
    Field2D.toGraphCardinalDirections(field) {
      case ((fromC, fromV), d, (toC, toV)) =>
        Set(fromV, toV).forall(_ == Path).option(1L)
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
