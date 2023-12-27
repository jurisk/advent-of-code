package jurisk.geometry

import cats.implicits._
import jurisk.math.Matrix2x2

object Coords2D {
  def apply(x: Int, y: Int): Coords2D = Coordinates2D[Int](x, y)
  def of(x: Int, y: Int): Coords2D    = Coords2D(x, y)

  val Zero: Coords2D = Coordinates2D.zero[Int]

  def parse(s: String): Coords2D = Coordinates2D.parse[Int](s)

  implicit val readingOrdering: Ordering[Coords2D] =
    Coordinates2D.readingOrdering[Int]

  def allPointsInclusive(a: Coords2D, b: Coords2D): Seq[Coords2D] =
    Coordinates2D.allPointsInclusive(a, b)

  private def adjacentCircularIndices[T](
    seq: IndexedSeq[T]
  ): Seq[(Int, Int)] = {
    val n = seq.length

    (0 to n)
      .map(_ % n)
      .toList
      .sliding2
  }

  private def adjacentCircularValues[T](seq: IndexedSeq[T]): Seq[(T, T)] =
    adjacentCircularIndices(seq).map { case (a, b) =>
      (seq(a), seq(b))
    }

  // https://en.wikipedia.org/wiki/Shoelace_formula#Shoelace_formula
  def areaOfSimplePolygon(seq: IndexedSeq[Coords2D]): Double = {
    val determinants = adjacentCircularValues(seq)
      .map { case (a, b) =>
        Matrix2x2(
          a.x.toDouble,
          b.x.toDouble,
          a.y.toDouble,
          b.y.toDouble,
        ).determinant
      }

    determinants.sum / 2.0
  }

  def calculateBoundaryPoints(seq: IndexedSeq[Coords2D]): Long =
    adjacentCircularValues(seq).map { case (a, b) =>
      assert((a.x == b.x) || (a.y == b.y))
      a.manhattanDistance(b)
    }.sum

  def interiorPointsExcludingBoundary(seq: IndexedSeq[Coords2D]): Long = {
    val boundaryPoints = calculateBoundaryPoints(seq)

    // https://en.wikipedia.org/wiki/Shoelace_formula
    val area = Coords2D.areaOfSimplePolygon(seq)

    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    (area - (boundaryPoints.toDouble / 2.0) + 1.0).toLong
  }

  def interiorPointsIncludingBoundary(seq: IndexedSeq[Coords2D]): Long =
    interiorPointsExcludingBoundary(seq) + calculateBoundaryPoints(seq)
}
