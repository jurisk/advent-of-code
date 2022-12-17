package jurisk.algorithms.pathfinding

import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

import scala.collection.mutable

object FloydWarshall {
  def allPairsDistances[N, C: Numeric](
    nodes: Seq[N],
    costFunction: (N, N) => Option[C],
  ): Map[(N, N), C] = {
    val Zero = implicitly[Numeric[C]].zero

    val dist: mutable.Map[(N, N), C] = mutable.Map.empty
    for {
      u <- nodes
      v <- nodes
    }
      costFunction(u, v) foreach { cost =>
        dist.update((u, v), cost)
      }

    for {
      v <- nodes
    } dist.update((v, v), Zero)

    for {
      k <- nodes
      i <- nodes
      j <- nodes
    } {
      val pathThroughK = (dist.get((i, k)), dist.get((k, j))) match {
        case (Some(ik), Some(kj)) => Some(ik + kj)
        case _                    => None
      }

      pathThroughK foreach { ikj =>
        val shouldUpdate = dist.get((i, j)) match {
          case Some(ij) if ij > ikj => true
          case None                 => true
          case _                    => false
        }

        if (shouldUpdate) {
          dist.update((i, j), ikj)
        }
      }

    }

    dist.toMap
  }
}
