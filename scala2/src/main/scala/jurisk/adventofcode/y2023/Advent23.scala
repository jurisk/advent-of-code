package jurisk.adventofcode.y2023

import cats.implicits.toFunctorOps
import jurisk.adventofcode.y2023.Advent23.Square.{Forest, Path, Slope}
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra, FloydWarshall}
import jurisk.collections.{BiMap, SetOfTwo}
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.optimization.Optimizer
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Advent23 {
  type Part1Input = Field2D[Square]

  sealed trait Square extends Product with Serializable
  object Square {
    case object Forest extends Square
    case object Path extends Square
    final case class Slope(direction: CardinalDirection2D) extends Square
  }

  def parse(input: String): Part1Input =
    Field2D.parseFromBiMap(input, BiMap(
      '#' <-> Forest,
      '.' <-> Path,
      '>' <-> Slope(Direction2D.E),
      'v' <-> Slope(Direction2D.S),
      '^' <-> Slope(Direction2D.N),
      '<' <-> Slope(Direction2D.W),
    ))

  final case class State(
    visited: List[Coords2D],
    steps: Int,
    current: Coords2D,
  ) {
    def next(field: Part1Input): List[State] = {
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
          visited = c :: visited,
          steps = steps + 1,
          current = c
        )
      }
    }
  }

  def solve1(data: Part1Input): Int = {
    val startCoords = data.topRowCoords.find(c => data.at(c).contains(Path)).get
    val goalCoords = data.bottomRowCoords.find(c => data.at(c).contains(Path)).get

    val startState = State(
      visited = List(startCoords),
      steps = 0,
      current = startCoords,
    )

    val reachable = Bfs.bfsReachable[State](
      startState,
      _.next(data),
    )

    val best = reachable.filter(_.current == goalCoords).maxBy(_.steps)
    println(best.visited.length)
    assert(best.visited.allDistinct)
    best.visited.reverse.foreach(println)
    best.steps
  }

  def part1(data: Part1Input): Long = solve1(data)

  def part2Crude(data: Part1Input): Long = {
    val updated = data.map {
      case Square.Forest => Square.Forest
      case Square.Path => Square.Path
      case Slope(_) => Square.Path
    }

    solve1(updated)
  }

  final case class Edge(vertices: SetOfTwo[VertexId], distance: Long) {
    def other(vertexId: VertexId): VertexId = {
      (vertices.underlying - vertexId).toList.singleResultUnsafe
    }
  }

  type VertexId = Int
  final case class UndirectedGraph[L](
    labelToIndexMap: BiMap[L, VertexId],
    edges: Set[Edge],
  ) {
    val allVertices: Seq[VertexId] = labelToIndexMap.rightKeys.toSeq
    val edgesFor: Map[VertexId, Set[Edge]] = allVertices.map { vertex =>
      vertex -> edges.filter(_.vertices.contains(vertex))
    }.toMap

//    def areConnected(a: VertexId, b: VertexId): Option[Edge] = edges.find(_.vertices == SetOfTwo(a, b))
    def connectedTo(vertexId: VertexId): Seq[VertexId] = edges.filter(_.vertices.contains(vertexId)).toList.map(_.other(vertexId))

    def connectors: Iterable[VertexId] = labelToIndexMap.rightKeys.filter(v => edgesFor(v).size > 2)
  }

  private def printDot(graph: UndirectedGraph[Coords2D], start: VertexId, goal: VertexId): Unit = {
    def vertexName(v: VertexId): String = {
      val c = graph.labelToIndexMap.rightToLeftUnsafe(v)
      s"x${c.x}y${c.y}"
    }

    println("graph G {")
    println()
    println(s"""${vertexName(start)} [color="green"]""")
    println(s"""${vertexName(goal)} [color="red"]""")

    graph.edges.foreach { e =>
      val (a, b) = e.vertices.tupleInArbitraryOrder
      println(s"""${vertexName(a)} -- ${vertexName(b)} [ label="${e.distance}" ]""")
    }

    println("}")
    println()
  }

  def simplifyGraph(graph: UndirectedGraph[Coords2D], start: VertexId, goal: VertexId): UndirectedGraph[Coords2D] = {
    val connectors = graph.connectors.toSet + start + goal
    println(connectors.size)

    println(connectors.map(graph.labelToIndexMap.rightToLeftUnsafe))

    var newEdges = Set.empty[Edge]

    connectors foreach { connector =>
      def helper(v: VertexId) = if (v == connector || !connectors.contains(v)) {
        graph.connectedTo(v).toList
      } else {
        Nil
      }

      val reachable = Bfs.bfsReachable[VertexId](connector, helper)

      reachable.filterNot(_ == connector) foreach { n =>
        if (connectors.contains(n)) {
          val distance = Bfs.bfsLength[VertexId](connector, helper, _ == n).get
          val edge = Edge(SetOfTwo(connector, n), distance)
          newEdges += edge
        }
      }
    }

    val labelToIndexMap = connectors.map { connector =>
      graph.labelToIndexMap.rightToLeftUnsafe(connector) -> connector
    }

    UndirectedGraph[Coords2D](
      labelToIndexMap = BiMap.from(labelToIndexMap),
      edges = newEdges,
    )
  }

  final case class State2(
    visited: Set[VertexId],
    steps: Long,
    current: VertexId,
  ) {
    def next(graph: UndirectedGraph[Coords2D]): List[State2] = {
      graph.edgesFor(current).flatMap { e =>
        val other = e.other(current)
        if (visited.contains(other)) {
          Nil
        } else {
          State2(
            visited = visited + other,
            steps = steps + e.distance,
            current = other,
          ) :: Nil
        }
      }.toList
    }
  }
  def solveState2Method(graph: UndirectedGraph[Coords2D], start: VertexId, goal: VertexId): Long = {
    val startState = State2(
      visited = Set(start),
      steps = 0,
      current = start,
    )

    var best = 0L
    Bfs.bfsVisitAll[State2](
      startState,
      _.next(graph),
      state => {
        if (state.current == goal) {
          if (state.steps > best) {
            best = state.steps
            println(s"$best: $state")
          }
        }
      }
    )
    best
  }

  def solve2Backtracking(graph: UndirectedGraph[Coords2D], start: VertexId, goal: VertexId): Long = {
    var best = 0L
    var visited = mutable.BitSet.fromSpecific(start :: Nil)

    def backtrack(current: VertexId, steps: Long): Unit = {
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
    }

    backtrack(start, 0)

    best
  }

  def solve2Optimizer() = {
//
//    implicit val o = Optimizer.z3()
//    import o._
//
//    graph.edges foreach { edge =>
//
//    }
    ???
  }

  def solve2(graph: UndirectedGraph[Coords2D], start: VertexId, goal: VertexId): Long = {
    val simplified = simplifyGraph(graph, start, goal)

    printDot(simplified, start, goal)

    solve2Backtracking(simplified, start, goal)
  }

  def part2(data: Part1Input): Long = {
    val field = data.map {
      case Square.Forest => Square.Forest
      case Square.Path => Square.Path
      case Slope(_) => Square.Path
    }

    val startCoords = field.topRowCoords.find(c => field.at(c).contains(Path)).get
    val goalCoords = field.bottomRowCoords.find(c => field.at(c).contains(Path)).get

    var nextIndex = 0
    var labelToIndexMap: Map[Coords2D, VertexId] = Map.empty
    field.allCoords.foreach { c =>
      field.at(c).foreach {
        case Square.Forest =>
        case _ =>
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

    val graph = UndirectedGraph(BiMap.from(labelToIndexMap), edges)
    solve2(graph, labelToIndexMap(startCoords), labelToIndexMap(goalCoords))
  }

  def parseFile(fileName: String): Part1Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/23$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Part1Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
