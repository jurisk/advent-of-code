package jurisk.geometry

object BigIntCoords2D {
  def apply(x: BigInt, y: BigInt): BigIntCoords2D =
    Coordinates2D.of[BigInt](x, y)
}
