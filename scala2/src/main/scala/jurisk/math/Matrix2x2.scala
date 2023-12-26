package jurisk.math

import scala.math.Numeric.Implicits.infixNumericOps

case class Matrix2x2[N: Numeric](a: N, b: N, c: N, d: N) {
  override def toString: String =
    s"""$a $b
       |$c $d
       |
       |""".stripMargin

  def determinant: N = a * d - b * c
}
