package jurisk.geometry

import cats.implicits._

final case class SparseField[T](points: Map[Coords2D, T]) {
  def toDebugRepresentation(
    display: Option[T] => Char,
    limit: Int,
  ): String = {
    val boundingBox = Coords2D.boundingBoxInclusive(points.keys)
    val xMin        = boundingBox.topLeft.x.value
    val yMin        = boundingBox.topLeft.y.value
    val xMax        = boundingBox.bottomRight.x.value
    val yMax        = boundingBox.bottomRight.y.value
    val xSize       = xMax - xMin + 1
    val ySize       = yMax - yMin + 1

    if ((xSize > limit) || (ySize > limit)) {
      s"Too large: ${xSize}x$ySize or ${xSize.toLong * ySize} with bounding box $boundingBox"
    } else {
      val buffers = Array.fill(ySize)(Array.fill(xSize)(display(none)))

      points foreach { case (point, value) =>
        buffers(point.y.value - yMin)(point.x.value - xMin) =
          display(value.some)
      }

      mergeSeqSeqChar(buffers.map(_.toList))
    }
  }
}

final case class SparseBooleanField(points: Set[Coords2D], limit: Int) {
  def toDebugRepresentation: String =
    SparseField(points.map(_ -> ()).toMap).toDebugRepresentation(
      {
        case Some(()) => '▓'
        case None     => '░'
      },
      limit,
    )
}
