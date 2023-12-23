package jurisk.graph

import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.{BiMap, SetOfTwo}
import jurisk.geometry.Coords2D
import jurisk.graph.Graph.{Edge, VertexId}
import jurisk.utils.CollectionOps.IterableOps

// TODO:  Improve this, use a better adjacencySets: IndexedSeq[Set[VertexId]]
//        and also extract trait.
final class Graph[L](
  private val labelToIndexMap: BiMap[L, VertexId],
  private val edges: Set[Edge],
) {
  private val allVertices: Seq[VertexId]            = labelToIndexMap.rightKeys.toSeq
  private val vertexEdges: Map[VertexId, Set[Edge]] = allVertices.map {
    vertex =>
      vertex -> edges.filter(_.vertices.contains(vertex))
  }.toMap

  def edgesFor(v: VertexId): Seq[(VertexId, Long)] =
    vertexEdges(v).map { e =>
      e.other(v) -> e.distance
    }.toSeq

  def labelToVertex(label: L): VertexId =
    labelToIndexMap.leftToRightUnsafe(label)

  def connectedTo(vertexId: VertexId): Seq[VertexId] =
    edges.filter(_.vertices.contains(vertexId)).toList.map(_.other(vertexId))

  // TODO: This is terrible, improve it, possibly rename
  def simplify(doNotTouch: Set[VertexId]): Graph[L] = {
    val nonOptimisibleVertices: Iterable[VertexId] =
      labelToIndexMap.rightKeys.filter(v => vertexEdges(v).size != 2)

    val connectors = nonOptimisibleVertices.toSet ++ doNotTouch
    println(connectors.size)

    println(connectors.map(labelToIndexMap.rightToLeftUnsafe))

    var newEdges = Set.empty[Edge]

    connectors foreach { connector =>
      def helper(v: VertexId) = if (v == connector || !connectors.contains(v)) {
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

    new Graph[L](BiMap.from(filteredLabelMap), newEdges)
  }
}

object Graph {
  type VertexId = Int

  final private case class Edge(vertices: SetOfTwo[VertexId], distance: Long) {
    def other(vertexId: VertexId): VertexId =
      (vertices.underlying - vertexId).toList.singleResultUnsafe
  }

  def undirected[L](edges: Set[(SetOfTwo[L], Long)]): Graph[L] = {
    val labelToIndexMap: Map[L, VertexId] = edges
      .flatMap { case (s, _) =>
        s.underlying
      }
      .toList
      .zipWithIndex
      .toMap

    val newEdges: Set[Edge] = edges.map { case (s, d) =>
      val (a, b) = s.tupleInArbitraryOrder
      Edge(SetOfTwo(labelToIndexMap(a), labelToIndexMap(b)), d)
    }

    new Graph[L](BiMap.from(labelToIndexMap), newEdges)
  }

  def toDot(
    graph: Graph[Coords2D],
    start: VertexId,
    goal: VertexId,
  ): String = {
    def vertexName(v: VertexId): String = {
      val c = graph.labelToIndexMap.rightToLeftUnsafe(v)
      s"x${c.x}y${c.y}"
    }

    val vertices = graph.edges.map { e =>
      val (a, b) = e.vertices.tupleInArbitraryOrder
      s"""${vertexName(a)} -- ${vertexName(b)} [ label="${e.distance}" ]"""
    }

    s"""graph G {
       |
       |${vertexName(start)} [color="green"]
       |${vertexName(goal)} [color="red"]
       |
       |${vertices.toList.sorted.mkString("\n")}
       |}
       |""".stripMargin
  }
}
