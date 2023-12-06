package jurisk.math

import jurisk.math.HasSqrt.SqrtOps

import scala.math.Fractional.Implicits._

final case class QuadraticEquation[N: Fractional: HasSqrt](a: N, b: N, c: N) {
  private val One  = implicitly[Numeric[N]].one
  private val Two  = One + One
  private val Four = Two + Two

  def roots: List[N] = {
    val discriminant = b * b - Four * a * c

    discriminant.sqrt match {
      case Some(sqrtD) =>
        val root1 = (-b + sqrtD) / (Two * a)
        val root2 = (-b - sqrtD) / (Two * a)
        List(root1, root2).distinct.sorted

      case None => Nil
    }
  }
}
