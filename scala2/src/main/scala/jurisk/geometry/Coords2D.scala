package jurisk.geometry

import cats.implicits._

object Coords2D {
  def apply(x: Int, y: Int): Coords2D = Coordinates2D[Int](x, y)
  def of(x: Int, y: Int): Coords2D    = Coords2D(x, y)

  val Zero: Coords2D = Coordinates2D.zero[Int]

  // TODO: Make Area2D generic on N: Integral and then move this to Coordinates2D
  def boundingBoxInclusive(coords: Iterable[Coords2D]): Area2D = {
    val xList = coords.map(_.x)
    val minX  = xList.min
    val maxX  = xList.max
    val yList = coords.map(_.y)
    val minY  = yList.min
    val maxY  = yList.max
    Area2D(Coordinates2D.of(minX, minY), Coordinates2D.of(maxX, maxY))
  }

  def parse(s: String): Coords2D = Coordinates2D.parse[Int](s)

  implicit val readingOrdering: Ordering[Coords2D] =
    Coordinates2D.readingOrdering[Int]

  def allPointsInclusive(a: Coords2D, b: Coords2D): List[Coords2D] =
    Coordinates2D.allPointsInclusive(a, b)

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
