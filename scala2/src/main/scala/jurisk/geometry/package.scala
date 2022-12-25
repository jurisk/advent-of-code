package jurisk

package object geometry {
  private[geometry] def mergeSeqSeqChar(data: Seq[Seq[Char]]): String =
    data.map(_.mkString).map(_ + '\n').mkString
}
