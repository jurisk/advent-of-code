package jurisk.geometry

// X and Y do not "extends AnyVal" on purpose because of type erasure issues
final case class X(value: Int) {
  def *(n: Int): X = X(value * n)
  def /(n: Int): X = X(value / n)

  def <(other: X): Boolean = value < other.value
  def >(other: X): Boolean = value > other.value

  def +(other: X): X   = this + other.value
  def -(other: X): X   = this - other.value
  def +(other: Int): X = X(value + other)
  def -(other: Int): X = X(value - other)
  def abs: Int         = Math.abs(value)

  override def toString: String = value.toString
}

final case class Y(value: Int) {
  def *(n: Int): Y = Y(value * n)
  def /(n: Int): Y = Y(value / n)

  def <(other: Y): Boolean = value < other.value
  def >(other: Y): Boolean = value > other.value

  def +(other: Y): Y   = this + other.value
  def -(other: Y): Y   = this - other.value
  def +(other: Int): Y = Y(value + other)
  def -(other: Int): Y = Y(value - other)
  def abs: Int         = Math.abs(value)

  override def toString: String = value.toString
}

final case class Coords2D(x: X, y: Y) {
  def +(other: Coords2D): Coords2D =
    Coords2D(x + other.x, y + other.y)

  def -(other: Coords2D): Coords2D =
    Coords2D(x - other.x, y - other.y)

  def *(n: Int): Coords2D =
    Coords2D(x * n, y * n)

  def manhattanDistanceToOrigin: Int =
    Math.abs(x.value) + Math.abs(y.value)

  def manhattanDistance(other: Coords2D): Int =
    (this - other).manhattanDistanceToOrigin

  def adjacent4: List[Coords2D] = neighbours(includeDiagonal = false)
  def adjacent8: List[Coords2D] = neighbours(includeDiagonal = true)

  def neighbours(includeDiagonal: Boolean): List[Coords2D] = {
    val directions = if (includeDiagonal) {
      Direction2D.AllDirections
    } else {
      Direction2D.CardinalDirections
    }

    directions.map(d => this + d.diff)
  }

  override def toString: String = s"($x, $y)"
}

object Coords2D {
  val Zero: Coords2D = Coords2D.of(0, 0)

  def of(x: Int, y: Int): Coords2D =
    Coords2D(X(x), Y(y))

  def boundingBoxInclusive(coords: Iterable[Coords2D]): Area2D = {
    val xList = coords.map(_.x.value)
    val minX  = xList.min
    val maxX  = xList.max
    val yList = coords.map(_.y.value)
    val minY  = yList.min
    val maxY  = yList.max
    Area2D(X(minX), Y(minY), maxX - minX + 1, maxY - minY + 1)
  }

  def parse(s: String): Coords2D = {
    val Array(a, b) = s.split(",")
    Coords2D.of(a.trim.toInt, b.trim.toInt)
  }

  def allPointsInclusive(a: Coords2D, b: Coords2D): List[Coords2D] =
    if (a.x == b.x) {
      (Math.min(a.y.value, b.y.value) to Math.max(
        a.y.value,
        b.y.value,
      )).toList map { y =>
        Coords2D.of(a.x.value, y)
      }
    } else if (a.y == b.y) {
      (Math.min(a.x.value, b.x.value) to Math.max(
        a.x.value,
        b.x.value,
      )).toList map { x =>
        Coords2D.of(x, a.y.value)
      }

    } else {
      sys.error("Unexpected")
    }
}
