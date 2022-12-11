package jurisk

package object geometry {
  implicit class StringOps(s: String) {
    def toX: X = X(s.toInt)
    def toY: Y = Y(s.toInt)
  }

  private[geometry] def mergeSeqSeqChar(data: Seq[Seq[Char]]): String =
    data.map(_.mkString).map(_ + '\n').mkString
}
