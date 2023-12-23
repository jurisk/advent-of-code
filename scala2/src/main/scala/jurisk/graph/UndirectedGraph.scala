package jurisk.graph

import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.BiMap
import jurisk.collections.SetOfTwo
import jurisk.geometry.Coords2D
import jurisk.graph.UndirectedGraph.VertexId
import jurisk.utils.CollectionOps.IterableOps

final case class Edge(vertices: SetOfTwo[VertexId], distance: Long) {
  def other(vertexId: VertexId): VertexId =
    (vertices.underlying - vertexId).toList.singleResultUnsafe
}

// TODO: Improve this
final case class UndirectedGraph[L](
  private val labelToIndexMap: BiMap[L, VertexId],
  private val edges: Set[Edge],
) {
  private val allVertices: Seq[VertexId]         = labelToIndexMap.rightKeys.toSeq
  val edgesFor: Map[VertexId, Set[Edge]] = allVertices.map { vertex =>
    vertex -> edges.filter(_.vertices.contains(vertex))
  }.toMap

  def labelToVertex(label: L): VertexId = {
    labelToIndexMap.leftToRightUnsafe(label)
  }

  def connectedTo(vertexId: VertexId): Seq[VertexId] =
    edges.filter(_.vertices.contains(vertexId)).toList.map(_.other(vertexId))

  // TODO: This is terrible, improve it, possibly rename
  def simplify(doNotTouch: Set[VertexId]): UndirectedGraph[L] = {
    val nonOptimisibleVertices: Iterable[VertexId] =
      labelToIndexMap.rightKeys.filter(v => edgesFor(v).size != 2)

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

    UndirectedGraph[L](BiMap.from(filteredLabelMap), newEdges)
  }
}

object UndirectedGraph {
  type VertexId = Int

  def toDot(
    graph: UndirectedGraph[Coords2D],
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
