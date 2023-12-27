package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.geometry.Coordinates2D
import jurisk.geometry.Coords2D
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput.parseFileLines
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent06 {
  def readFileAndParse(fileName: String): List[Coords2D] =
    parseFileLines(fileName, _.parseCoords2D)

  def part1(points: List[Coords2D]): Int = {
    val boundingBox = Coordinates2D.boundingBoxInclusive(points)
    println(s"Bounding box: $boundingBox")

    def onEdgeOfBoundingBox(point: Coords2D): Boolean =
      ((point.x == boundingBox.left)
        || (point.y == boundingBox.top)
        || (point.x == boundingBox.left + boundingBox.width - 1)
        || (point.y == boundingBox.top + boundingBox.height - 1))

    def closestPointTo(point: Coords2D): Option[Coords2D] = {
      val distances: Map[Coords2D, Int]      = points.map { c: Coords2D =>
        c -> point.manhattanDistance(c)
      }.toMap
      val shortestDistance                   = distances.values.min
      val atShortestDistance: List[Coords2D] =
        distances.filter { case (_, v) => v == shortestDistance }.keys.toList

      atShortestDistance match {
        case closestPoint :: Nil => Some(closestPoint)
        case _                   => None
      }
    }

    val allPoints  = boundingBox.points
    val edgePoints = allPoints.filter(onEdgeOfBoundingBox)
    val inf        = edgePoints.toSet.flatMap(closestPointTo)

    val closests = allPoints flatMap closestPointTo

    println(s"${inf.size} points have areas that are infinite: $inf")

    closests.filterNot(inf.contains).counts.values.max
  }

  def part2(points: List[Coords2D], limit: Int): Int =
    Coordinates2D.boundingBoxInclusive(points).points.count { point =>
      val distanceToAll = points.map(_.manhattanDistance(point)).sum
      distanceToAll < limit
    }

  def main(args: Array[String]): Unit = {
    val test = readFileAndParse("2018/06-test.txt")
    val real = readFileAndParse("2018/06.txt")

    part1(test) shouldEqual 17
    part1(real) shouldEqual 2342

    part2(test, 32) shouldEqual 16
    part2(real, 10000) shouldEqual 43302
  }
}
