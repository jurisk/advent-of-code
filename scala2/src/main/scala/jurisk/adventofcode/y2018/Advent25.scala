package jurisk.adventofcode.y2018

import jurisk.geometry.Coords4D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent25 {
  type Parsed = List[Coords4D]

  def parse(data: String): Parsed =
    data.parseLines(Coords4D.parse)

  private def belongsTo(
    point: Coords4D,
    constellation: Set[Coords4D],
  ): Boolean =
    constellation.exists { c =>
      (point manhattanDistance c) <= 3
    }

  private def updateConstellations(
    constellations: Set[Set[Coords4D]],
    point: Coords4D,
  ): Set[Set[Coords4D]] = {
    val needsToJoin                      = constellations.filter { constellation =>
      belongsTo(point, constellation)
    }
    val constellationsWithoutNeedsToJoin = constellations -- needsToJoin

    needsToJoin.toList match {
      case Nil           => constellations + Set(point)
      case single :: Nil =>
        constellationsWithoutNeedsToJoin + (single + point)
      case more          =>
        val merged = more.foldLeft(Set(point))(_ union _)
        constellationsWithoutNeedsToJoin + merged
    }
  }

  def part1(points: Parsed): Int = {
    require(points.distinct.size == points.size)

    val results = points.foldLeft[Set[Set[Coords4D]]](Set.empty) {
      case (acc, point) =>
        updateConstellations(acc, point)
    }

    results.size
  }

  def main(args: Array[String]): Unit = {
    val realData = readFileText("2018/25.txt")

    val real = parse(realData)

    part1(real) shouldEqual 305
  }
}
