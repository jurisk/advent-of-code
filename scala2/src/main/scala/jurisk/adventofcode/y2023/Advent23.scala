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
    visited: List[Coords2D],
    steps: Int,
    current: Coords2D,
  ) {
    def next(field: Input): List[State] = {
      val candidates = field.atOrElse(current, Forest) match {
        case Square.Forest    => "wtf".fail
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
          visited = c :: visited,
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

  def part1(data: Input): Long = solve1(data)

  def part2Crude(data: Input): Long = {
    val updated = convertPart1ToPart2(data)

    solve1(updated)
  }

  final case class Edge(vertices: SetOfTwo[VertexId], distance: Long) {
    def other(vertexId: VertexId): VertexId =
      (vertices.underlying - vertexId).toList.singleResultUnsafe
  }

  // TODO: Move to library & optimise it.
  private type VertexId = Int
  final case class UndirectedGraph[L](
    labelToIndexMap: BiMap[L, VertexId],
    edges: Set[Edge],
  ) {
    val allVertices: Seq[VertexId]         = labelToIndexMap.rightKeys.toSeq
    val edgesFor: Map[VertexId, Set[Edge]] = allVertices.map { vertex =>
      vertex -> edges.filter(_.vertices.contains(vertex))
    }.toMap

    def connectedTo(vertexId: VertexId): Seq[VertexId] =
      edges.filter(_.vertices.contains(vertexId)).toList.map(_.other(vertexId))

    private def verticesWithMoreThanTwoEdges: Iterable[VertexId] =
      labelToIndexMap.rightKeys.filter(v => edgesFor(v).size > 2)

    // TODO: This is terrible, improve it, possibly rename
    def simplify(doNotTouch: Set[VertexId]): UndirectedGraph[L] = {
      val connectors = verticesWithMoreThanTwoEdges.toSet ++ doNotTouch
      println(connectors.size)

      println(connectors.map(labelToIndexMap.rightToLeftUnsafe))

      var newEdges = Set.empty[Edge]

      connectors foreach { connector =>
        def helper(v: VertexId) = if (
          v == connector || !connectors.contains(v)
        ) {
          connectedTo(v).toList
        } else {
          Nil
        }

        val reachable = Bfs.bfsReachable[VertexId](connector, helper)

        reachable.filterNot(_ == connector) foreach { n =>
          if (connectors.contains(n)) {
            val distance =
              Bfs.bfsLength[VertexId](connector, helper, _ == n).get
            val edge     = Edge(SetOfTwo(connector, n), distance)
            newEdges += edge
          }
        }
      }

      val filteredLabelMap = connectors.map { connector =>
        labelToIndexMap.rightToLeftUnsafe(connector) -> connector
      }

      UndirectedGraph[L](BiMap.from(filteredLabelMap), newEdges)
    }
  }

  // TODO: move out
  private def printDot(
    graph: UndirectedGraph[Coords2D],
    start: VertexId,
    goal: VertexId,
  ): Unit = {
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
      println(
        s"""${vertexName(a)} -- ${vertexName(b)} [ label="${e.distance}" ]"""
      )
    }

    println("}")
    println()
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

  def solve2(
    graph: UndirectedGraph[Coords2D],
    start: VertexId,
    goal: VertexId,
  ): Long = {
    val simplified = graph.simplify(Set(start, goal))

    printDot(simplified, start, goal)

    solve2Backtracking(simplified, start, goal)
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
      field.topRowCoords.find(c => field.at(c).contains(Path)).get
    val goalCoords  =
      field.bottomRowCoords.find(c => field.at(c).contains(Path)).get

    val graph = fieldToGraph(field)
    solve2(
      graph,
      graph.labelToIndexMap.leftToRightUnsafe(startCoords),
      graph.labelToIndexMap.leftToRightUnsafe(goalCoords),
    )
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
