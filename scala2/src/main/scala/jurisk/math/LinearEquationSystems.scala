package jurisk.math

import mouse.all.booleanSyntaxMouse

import scala.math.Integral.Implicits.infixIntegralOps

object LinearEquationSystems {
  // Solve a system of two linear equations with two variables, only integer solutions are supported:
  // a * x + b * y = e
  // c * x + d * y = f
  def solveTwoVariablesInteger[N: Integral](
    a: N,
    b: N,
    c: N,
    d: N,
    e: N,
    f: N,
  ): Option[(N, N)] = {
    val n = implicitly[Integral[N]]

    def div(dividend: N, divisor: N): Option[N] =
      (dividend % divisor == n.zero).option(dividend / divisor)

    val det = a * d - b * c
    if (det == n.zero) None
    else {
      val x = div(e * d - b * f, det)
      val y = div(a * f - e * c, det)
      (x, y) match {
        case (Some(x), Some(y)) => Some((x, y))
        case _                  => None
      }
    }
  }
}
