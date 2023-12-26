package jurisk.geometry

import cats.implicits._
import jurisk.math.Enumerated
import jurisk.utils.Parsing.StringOps

import scala.math.Numeric.Implicits.infixNumericOps

final case class Coordinates2D[N: Numeric](x: N, y: N) {
  def +(other: Coordinates2D[N]): Coordinates2D[N] =
    Coordinates2D(x + other.x, y + other.y)

  def -(other: Coordinates2D[N]): Coordinates2D[N] =
    Coordinates2D(x - other.x, y - other.y)

  def *(n: N): Coordinates2D[N] =
    Coordinates2D(x * n, y * n)

  def manhattanDistanceToOrigin(implicit integral: Integral[N]): N =
    x.abs + y.abs

  def manhattanDistance(other: Coordinates2D[N])(implicit
    integral: Integral[N]
  ): N =
    (this - other).manhattanDistanceToOrigin

  def adjacent4(implicit integral: Integral[N]): List[Coordinates2D[N]] =
    neighbours(includeDiagonal = false)
  def adjacent8(implicit integral: Integral[N]): List[Coordinates2D[N]] =
    neighbours(includeDiagonal = true)

  def neighbours(
    includeDiagonal: Boolean
  )(implicit integral: Integral[N]): List[Coordinates2D[N]] = {
    val _ =
      integral.zero // This is a hack to ensure that the compiler does not complain about the unused Integral requirement

    val directions = if (includeDiagonal) {
      Direction2D.AllDirections
    } else {
      Direction2D.CardinalDirections
    }

    directions.map(this + _)
  }

  override def toString: String = s"($x, $y)"

  def +(direction: Direction2D): Coordinates2D[N] = {
    val numeric   = implicitly[Numeric[N]]
    val diff      = direction.diff
    val converted =
      Coordinates2D.of[N](numeric.fromInt(diff.x), numeric.fromInt(diff.y))
    this + converted
  }

  def N: Coordinates2D[N]  = this + Direction2D.N
  def E: Coordinates2D[N]  = this + Direction2D.E
  def S: Coordinates2D[N]  = this + Direction2D.S
  def W: Coordinates2D[N]  = this + Direction2D.W
  def NE: Coordinates2D[N] = this + Direction2D.NE
  def NW: Coordinates2D[N] = this + Direction2D.NW
  def SE: Coordinates2D[N] = this + Direction2D.SE
  def SW: Coordinates2D[N] = this + Direction2D.SW
}

object Coordinates2D {
  implicit def readingOrdering[N: Ordering]: Ordering[Coordinates2D[N]] =
    Ordering[(N, N)].contramap(c => (c.y, c.x))

  def zero[N: Numeric]: Coordinates2D[N] = {
    val numeric = implicitly[Numeric[N]]
    Coordinates2D.of[N](numeric.zero, numeric.zero)
  }

  def of[N: Numeric](x: N, y: N): Coordinates2D[N] =
    Coordinates2D[N](x, y)

  def parse[N: Integral](s: String): Coordinates2D[N] = {
    val numeric     = implicitly[Numeric[N]]
    val Array(a, b) = s.split(",")
    val an: N       = numeric.parseString(a.trim) getOrElse s"Failed to parse $a".fail
    val bn: N       = numeric.parseString(b.trim) getOrElse s"Failed to parse $b".fail
    Coordinates2D.of[N](an, bn)
  }

  def allPointsInclusive[N: Integral: Enumerated](
    a: Coordinates2D[N],
    b: Coordinates2D[N],
  ): List[Coordinates2D[N]] = {
    import jurisk.math.Enumerated.EnumeratedOps
    val numeric          = implicitly[Numeric[N]]
    val min: (N, N) => N = numeric.min
    val max: (N, N) => N = numeric.max

    if (a.x == b.x) {
      (min(a.y, b.y) to max(
        a.y,
        b.y,
      )) map { y =>
        Coordinates2D.of(a.x, y)
      }
    } else if (a.y == b.y) {
      (min(a.x, b.x) to max(
        a.x,
        b.x,
      )) map { x =>
        Coordinates2D.of(x, a.y)
      }

    } else
      s"Expected $a and $b to have same x or y coordinates, but they do not".fail
  }

}
