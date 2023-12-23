package jurisk.graph

import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.{BiMap, SetOfTwo}
import jurisk.geometry.Coords2D
import jurisk.graph.Graph.{Distance, VertexId}
import jurisk.utils.CollectionOps.ArraySeqOps

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Graph[L](
  private val labels: ArraySeq[L],
  private val labelIndices: Map[L, VertexId],
  private val adjacency: ArraySeq[Set[(VertexId, Distance)]],
) {
  def allVertices: Seq[VertexId] = adjacency.indices

  def edgesFor(v: VertexId): Set[(VertexId, Distance)] =
    adjacency.lift(v).getOrElse(Set.empty)

  def labelFor(v: VertexId): L = labels(v)

  def labelToVertex(label: L): VertexId =
    labelIndices(label)

  def reachableFrom(from: VertexId): Set[VertexId] =
    edgesFor(from).map { case (n, _) => n }

  // TODO: This is terrible, improve it, possibly rename
  def simplify(doNotTouch: Set[VertexId])(implicit labelOrdering: Ordering[L], labelClassTag: ClassTag[L]): Graph[L] = {
    val nonOptimisibleVertices: Iterable[VertexId] =
      allVertices.filter(v => edgesFor(v).size != 2)

    val connectors = nonOptimisibleVertices.toSet ++ doNotTouch

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
              labels(connector),
              labels(n),
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

  def undirected[L: Ordering: ClassTag](edges: Set[(SetOfTwo[L], Long)]): Graph[L] = {
    val labels = edges
      .flatMap { case (s, _) =>
        s.toSet
      }
      .toSeq
      .sorted

    val labelIndices: Map[L, VertexId] = labels
      .zipWithIndex
      .toMap

    val e = edges.foldLeft(
      ArraySeq.fill(labelIndices.size)(Set.empty[(VertexId, Distance)])
    ) { case (acc, (s, d)) =>
      val (a, b) = s.tupleInArbitraryOrder
      val aIdx   = labelIndices(a)
      val bIdx   = labelIndices(b)
      acc
        .updatedWith(aIdx)(set => set + (bIdx -> d))
        .updatedWith(bIdx)(set => set + (aIdx -> d))
    }

    new Graph[L](ArraySeq.from(labels), labelIndices, e)
  }

  def toDotDigraph(
    graph: Graph[Coords2D],
    start: Coords2D,
    goal: Coords2D,
  ): String = {
    def coordsName(c: Coords2D): String = s"x${c.x}y${c.y}"

    def vertexName(v: VertexId): String = coordsName(
      graph.labelFor(v)
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
