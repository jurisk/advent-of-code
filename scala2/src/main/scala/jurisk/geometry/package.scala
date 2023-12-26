package jurisk

import cats.implicits._
import jurisk.math.Matrix2x2

import scala.math.Fractional.Implicits.infixFractionalOps

package object geometry {
  private[geometry] def mergeSeqSeqChar(data: Seq[Seq[Char]]): String =
    data.map(_.mkString).map(_ + '\n').mkString

  type Coords2D       = Coordinates2D[Int]
  type LongCoords2D   = Coordinates2D[Long]
  type BigIntCoords2D = Coordinates2D[BigInt]

  def visualizeBoolean(b: Boolean): Char = if (b) '█' else '░'

  // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
  def lineLineIntersectionGivenTwoPointsOnEachLine[N: Fractional](
    a1: Coordinates2D[N],
    a2: Coordinates2D[N],
    b1: Coordinates2D[N],
    b2: Coordinates2D[N],
  ): Option[Coordinates2D[N]] = {
    val Zero = implicitly[Fractional[N]].zero
    val One  = implicitly[Fractional[N]].one

    val x1 = a1.x
    val y1 = a1.y
    val x2 = a2.x
    val y2 = a2.y
    val x3 = b1.x
    val y3 = b1.y
    val x4 = b2.x
    val y4 = b2.y

    val x1_x2 = Matrix2x2(x1, One, x2, One).determinant
    val x3_x4 = Matrix2x2(x3, One, x4, One).determinant

    val y1_y2 = Matrix2x2(y1, One, y2, One).determinant
    val y3_y4 = Matrix2x2(y3, One, y4, One).determinant

    val bottom = Matrix2x2(x1_x2, y1_y2, x3_x4, y3_y4).determinant

    if (bottom == Zero) {
      none
    } else {
      val a = Matrix2x2(x1, y1, x2, y2).determinant
      val b = Matrix2x2(x3, y3, x4, y4).determinant

      val pxTop = Matrix2x2(
        a,
        x1_x2,
        b,
        x3_x4,
      ).determinant

      val px = pxTop / bottom

      val pyTop = Matrix2x2(
        a,
        y1_y2,
        b,
        y3_y4,
      ).determinant

      val py = pyTop / bottom

      Coordinates2D(px, py).some
    }
  }
}
