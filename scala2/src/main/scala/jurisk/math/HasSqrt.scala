package jurisk.math

import cats.implicits._

trait HasSqrt[T] {
  def sqrt(x: T): Option[T]
}

object HasSqrt {
  // Note - no BigDecimal support as it has no `sqrt` included out of the box.

  implicit object FloatHasSqrt extends HasSqrt[Float] {
    def sqrt(x: Float): Option[Float] = if (x >= 0) {
      math.sqrt(x.toDouble).toFloat.some
    } else {
      none
    }
  }

  implicit object DoubleHasSqrt extends HasSqrt[Double] {
    def sqrt(x: Double): Option[Double] = if (x >= 0) {
      math.sqrt(x).some
    } else {
      none
    }
  }

  implicit class SqrtOps[T](val x: T) extends AnyVal {
    def sqrt(implicit hasSqrt: HasSqrt[T]): Option[T] = hasSqrt.sqrt(x)
  }
}
