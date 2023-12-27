package jurisk.collections.immutable.graph

import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.immutable.SetOfTwo
import jurisk.geometry.Coords2D
import jurisk.utils.CollectionOps.ArraySeqOps

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import Graph.Distance
import Graph.VertexId

trait Graph[L] {
  def allVertices: Seq[VertexId]
  def allEdges: Seq[(VertexId, Distance, VertexId)]
  def vertexCount: Int
  def outgoingEdges(v: VertexId): Seq[(VertexId, Distance)]
  def labelFor(v: VertexId): L
  def labelToVertex(label: L): VertexId
  def simplify(doNotTouch: Set[VertexId]): Graph[L]
  def verticesReachableFrom(from: VertexId): Seq[VertexId]
}

final class GraphImpl[L: Ordering: ClassTag](
  private val labels: ArraySeq[L],
  private val labelIndices: Map[L, VertexId],
  private val adjacency: ArraySeq[Seq[(VertexId, Distance)]],
) extends Graph[L] {
  def vertexCount: Int           = adjacency.size
  def allVertices: Seq[VertexId] = adjacency.indices

  override def allEdges: Seq[(VertexId, Distance, VertexId)] =
    allVertices.flatMap { from =>
      outgoingEdges(from).map { case (to, distance) =>
        (from, distance, to)
      }
    }

  def outgoingEdges(v: VertexId): Seq[(VertexId, Distance)] =
    adjacency.lift(v).getOrElse(Seq.empty)

  def labelFor(v: VertexId): L = labels(v)

  def labelToVertex(label: L): VertexId =
    labelIndices(label)

  def verticesReachableFrom(from: VertexId): Seq[VertexId] =
    outgoingEdges(from).map { case (n, _) => n }

  // TODO:  This is really crude, improve it. Also it was written assuming an undirected graph. Either assert this,
  //        or make it work with directed ones (and then test with `Advent 2023-23-1`).
  //        Also, the part where it changes all `VertexId`-s is really error-prone, they should stay the same. Which
  //        means You probably need a different, slightly less effective representation - e.g. BiMap[VertexId, Label] again.
  def simplify(doNotTouch: Set[VertexId]): Graph[L] = {
    val nonOptimisibleVertices: Iterable[VertexId] =
      allVertices.filter(v => outgoingEdges(v).size != 2)

    val connectors = nonOptimisibleVertices.toSet ++ doNotTouch

    var newEdges = Set.empty[(SetOfTwo[L], Long)]

    connectors foreach { connector =>
      def helper(v: VertexId) = if (v == connector || !connectors.contains(v)) {
        verticesReachableFrom(v).toList
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

  def directed[L: Ordering: ClassTag](
    edges: Set[(L, Distance, L)]
  ): Graph[L] = {
    val labels = edges
      .flatMap { case (from, d, to) =>
        Set(from, to)
      }
      .toSeq
      .sorted

    val labelIndices: Map[L, VertexId] = labels.zipWithIndex.toMap

    val e = edges.foldLeft(
      ArraySeq.fill(labelIndices.size)(Set.empty[(VertexId, Distance)])
    ) { case (acc, (from, d, to)) =>
      val a = labelIndices(from)
      val b = labelIndices(to)
      acc.updatedWith(a)(set => set + (b -> d))
    }

    new GraphImpl[L](ArraySeq.from(labels), labelIndices, e.map(_.toSeq))
  }

  def undirected[L: Ordering: ClassTag](
    edges: Set[(SetOfTwo[L], Long)]
  ): Graph[L] = {
    val adapted = edges.flatMap { case (v, d) =>
      val (a, b) = v.tupleInArbitraryOrder
      Set(
        (a, d, b),
        (b, d, a),
      )
    }

    directed(adapted)
  }

  // TODO: Implement, reusing code with `toDotDigraph`, not tying to Coords2D
  def toDotUndirectedGraph(
    graph: Graph[Coords2D],
    start: Coords2D,
    goal: Coords2D,
  ): String =
    // TODO: Start by asserting it is indeed undirected

    ???

  // TODO: Instead of special `start` and `goal` nodes, you can just have a map of "colors"?
  // TODO: Don't tie to Coords2D
  def toDotDigraph(
    graph: Graph[Coords2D],
    start: Coords2D,
    goal: Coords2D,
  ): String = {
    def coordsName(c: Coords2D): String = s"x${c.x}y${c.y}"

    def vertexName(v: VertexId): String = coordsName(
      graph.labelFor(v)
    )

    val edges = graph.allEdges.map { case (from, d, to) =>
      s"""  ${vertexName(from)} -> ${vertexName(to)} [ label="$d" ]"""
    }

    s"""digraph G {
       |
       |    ${coordsName(start)} [color="green"]
       |    ${coordsName(goal)} [color="red"]
       |
       |${edges.toList.sorted.mkString("\n")}
       |}
       |""".stripMargin
  }
}
