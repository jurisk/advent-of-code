package jurisk.utils

trait ToInt[T] {
  def toInt(value: T): Int
}

trait FromInt[T] {
  def fromInt(value: Int): T
}

object ToIntInstances {
  implicit val intToInt: ToInt[Int] = identity
}

object FromIntInstances {
  implicit val intFromInt: FromInt[Int] = identity
}

trait ToIntSyntax {
  implicit class ToIntOps[T](value: T)(implicit ev: ToInt[T]) {
    def toInt: Int = ev.toInt(value)
  }
}

trait FromIntSyntax {
  implicit class FromIntOps(value: Int) {
    def fromInt[T](implicit ev: FromInt[T]): T = ev.fromInt(value)
  }
}

abstract class AllSyntax extends ToIntSyntax with FromIntSyntax

object conversions {
  object syntax extends AllSyntax
}
