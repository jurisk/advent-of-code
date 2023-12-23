package jurisk.graph

import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.{BiMap, SetOfTwo}
import jurisk.geometry.Coords2D
import jurisk.graph.Graph.{Distance, VertexId}
import jurisk.utils.CollectionOps.ArraySeqOps

import scala.collection.immutable.ArraySeq

final class Graph[L](
  private val labelToIndexMap: BiMap[L, VertexId],
  // TODO: labelIndices: Map[L, VertexId]
  // TODO: labels: ArraySeq[Label]
  private val adjacency: ArraySeq[Set[(VertexId, Distance)]],
) {
  def allVertices: Seq[VertexId] = adjacency.indices

  def edgesFor(v: VertexId): Set[(VertexId, Distance)] =
    adjacency.lift(v).getOrElse(Set.empty)

  def labelToVertex(label: L): VertexId =
    labelToIndexMap.leftToRightUnsafe(label)

  def reachableFrom(from: VertexId): Set[VertexId] =
    edgesFor(from).map { case (n, _) => n }

  // TODO: This is terrible, improve it, possibly rename
  def simplify(doNotTouch: Set[L]): Graph[L] = {
    val nonOptimisibleVertices: Iterable[VertexId] =
      labelToIndexMap.rightKeys.filter(v => edgesFor(v).size != 2)

    val connectors = nonOptimisibleVertices.toSet ++ doNotTouch.map(
      labelToIndexMap.leftToRightUnsafe
    )

    var newEdges = Set.empty[(SetOfTwo[L], Long)]

    connectors foreach { connector =>
      def helper(v: VertexId) = if (v == connector || !connectors.contains(v)) {
        reachableFrom(v).toList
      } else {
        Nil
      }

      val reachable = Bfs.bfsReachable[VertexId](connector, helper)

      reachable.filterNot(_ == connector) foreach { n =>
        if (connectors.contains(n)) {
          val distance: Long =
            Bfs.bfsLength[VertexId](connector, helper, _ == n).get
          val edge           = (
            SetOfTwo(
              labelToIndexMap.rightToLeftUnsafe(connector),
              labelToIndexMap.rightToLeftUnsafe(n),
            ),
            distance,
          )
          newEdges += edge
        }
      }
    }

    Graph.undirected(newEdges)
  }
}

object Graph {
  type VertexId = Int
  type Distance = Long

  def undirected[L](edges: Set[(SetOfTwo[L], Long)]): Graph[L] = {
    val labelToIndexMap: Map[L, VertexId] = edges
      .flatMap { case (s, _) =>
        s.toSet
      }
      .toList
      .zipWithIndex
      .toMap

    val e = edges.foldLeft(
      ArraySeq.fill(labelToIndexMap.size)(Set.empty[(VertexId, Distance)])
    ) { case (acc, (s, d)) =>
      val (a, b) = s.tupleInArbitraryOrder
      val aIdx   = labelToIndexMap(a)
      val bIdx   = labelToIndexMap(b)
      acc
        .updatedWith(aIdx)(set => set + (bIdx -> d))
        .updatedWith(bIdx)(set => set + (aIdx -> d))
    }

    new Graph[L](BiMap.from(labelToIndexMap), e)
  }

  def toDotDigraph(
    graph: Graph[Coords2D],
    start: Coords2D,
    goal: Coords2D,
  ): String = {
    def coordsName(c: Coords2D): String = s"x${c.x}y${c.y}"

    def vertexName(v: VertexId): String = coordsName(
      graph.labelToIndexMap.rightToLeftUnsafe(v)
    )

    val edges = graph.allVertices.flatMap { v =>
      graph.edgesFor(v).map { case (n, d) =>
        s"""  ${vertexName(v)} -> ${vertexName(n)} [ label="$d" ]"""
      }
    }

    s"""digraph G {
       |
       |${coordsName(start)} [color="green"]
       |${coordsName(goal)} [color="red"]
       |
       |${edges.toList.sorted.mkString("\n")}
       |}
       |""".stripMargin
  }
}
