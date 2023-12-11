package jurisk

package object geometry {
  private[geometry] def mergeSeqSeqChar(data: Seq[Seq[Char]]): String =
    data.map(_.mkString).map(_ + '\n').mkString

  type Coords2D       = Coordinates2D[Int]
  type LongCoords2D   = Coordinates2D[Long]
  type BigIntCoords2D = Coordinates2D[BigInt]

  def visualizeBoolean(b: Boolean): Char = if (b) '█' else '░'
}
