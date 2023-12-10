package jurisk.geometry

import cats.implicits._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

final case class Coords2D(x: Int, y: Int) {
  def +(other: Coords2D): Coords2D =
    Coords2D(x + other.x, y + other.y)

  def -(other: Coords2D): Coords2D =
    Coords2D(x - other.x, y - other.y)

  def *(n: Int): Coords2D =
    Coords2D(x * n, y * n)

  def manhattanDistanceToOrigin: Int =
    Math.abs(x) + Math.abs(y)

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

    directions.map(this + _)
  }

  override def toString: String = s"($x, $y)"

  def +(direction: Direction2D): Coords2D = this + direction.diff

  def N: Coords2D  = this + Direction2D.N
  def E: Coords2D  = this + Direction2D.E
  def S: Coords2D  = this + Direction2D.S
  def W: Coords2D  = this + Direction2D.W
  def NE: Coords2D = this + Direction2D.NE
  def NW: Coords2D = this + Direction2D.NW
  def SE: Coords2D = this + Direction2D.SE
  def SW: Coords2D = this + Direction2D.SW
}

object Coords2D {

  implicit val readingOrdering: Ordering[Coords2D] =
    Ordering[(Int, Int)].contramap(c => (c.y, c.x))

  val Zero: Coords2D = Coords2D.of(0, 0)

  def of(x: Int, y: Int): Coords2D =
    Coords2D(x, y)

  def boundingBoxInclusive(coords: Iterable[Coords2D]): Area2D = {
    val xList = coords.map(_.x)
    val minX  = xList.min
    val maxX  = xList.max
    val yList = coords.map(_.y)
    val minY  = yList.min
    val maxY  = yList.max
    Area2D(Coords2D.of(minX, minY), Coords2D.of(maxX, maxY))
  }

  def parse(s: String): Coords2D = {
    val Array(a, b) = s.split(",")
    Coords2D.of(a.trim.toInt, b.trim.toInt)
  }

  def allPointsInclusive(a: Coords2D, b: Coords2D): List[Coords2D] =
    if (a.x == b.x) {
      (Math.min(a.y, b.y) to Math.max(
        a.y,
        b.y,
      )).toList map { y =>
        Coords2D.of(a.x, y)
      }
    } else if (a.y == b.y) {
      (Math.min(a.x, b.x) to Math.max(
        a.x,
        b.x,
      )).toList map { x =>
        Coords2D.of(x, a.y)
      }

    } else
      s"Expected $a and $b to have same x or y coordinates, but they do not".fail

  // https://en.wikipedia.org/wiki/Shoelace_formula#Shoelace_formula
  def areaOfSimplePolygon(seq: IndexedSeq[Coords2D]): Double = {
    val n = seq.length

    val determinants = (0 to n)
      .map(_ % n)
      .toList
      .sliding2
      .map { case (a, b) =>
        (seq(a).x.toDouble * seq(b).y.toDouble) -
          (seq(a).y.toDouble * seq(b).x.toDouble)
      }

    (determinants.sum / 2.0).abs
  }

}
