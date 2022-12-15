package jurisk.adventofcode.y2022

import jurisk.geometry.{Coords2D, Field2D}
import jurisk.math.{InclusiveDiscreteInterval, NonOverlappingDiscreteIntervalSet}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._
import mouse.all._

object Advent15 {
  type Parsed = List[Entry]

  final case class Entry(sensor: Coords2D, closestBeacon: Coords2D) {
    def impossibleToHaveBeacon(c: Coords2D): Boolean =
      sensor.manhattanDistance(c) <= radius

    def minX: Int = Math.min(sensor.x.value, closestBeacon.x.value)
    def maxX: Int = Math.max(sensor.x.value, closestBeacon.x.value)

    def radius: Int = sensor manhattanDistance closestBeacon

    def intervalWithY(y: Int): Option[InclusiveDiscreteInterval] = {
      val dy    = Math.abs(sensor.y.value - y)
      val delta = radius - dy
      (delta >= 0) option InclusiveDiscreteInterval(
        sensor.x.value - delta,
        sensor.x.value + delta,
      )
    }
  }

  object Entry {
    def parse(s: String): Entry =
      s match {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
          Entry(
            Coords2D.of(sx.toInt, sy.toInt),
            Coords2D.of(bx.toInt, by.toInt),
          )
        case _                                                            => s"Unrecognized $s".fail
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Entry.parse)

  private def impossibleToHaveBeacon(data: Parsed, c: Coords2D): Boolean =
    data.exists(_.impossibleToHaveBeacon(c))

  private def impossibilityIntervalsInRow(
    data: Parsed,
    y: Int,
    from: Int,
    to: Int,
  ): NonOverlappingDiscreteIntervalSet =
    data.foldLeft(NonOverlappingDiscreteIntervalSet.createInclusive(from, to)) {
      case (acc, e) =>
        e intervalWithY y match {
          case Some(interval) => acc.subtract(interval)
          case None           => acc
        }
    }

  def part1(data: Parsed, y: Int): Int = {
    val minX = data.map(_.minX).min - data.map(_.radius).max
    val maxX = data.map(_.maxX).max + data.map(_.radius).max

    require(!impossibleToHaveBeacon(data, Coords2D.of(minX, y)))
    require(!impossibleToHaveBeacon(data, Coords2D.of(maxX, y)))

    val intervalSet = impossibilityIntervalsInRow(data, y, minX, maxX)
    val thisSize    = intervalSet.size
    val rangeSize   = maxX - minX
    rangeSize - thisSize
  }

  def printData(data: Parsed, maxCoords: Int): Unit = {
    val sensorsAt = data.map(_.sensor).toSet
    val beaconsAt = data.map(_.closestBeacon).toSet

    val field     = Field2D.ofSize(maxCoords + 1, maxCoords + 1, ' ')
    val filledOut = field.mapByCoords(c =>
      if (sensorsAt.contains(c)) {
        'S'
      } else if (beaconsAt.contains(c)) {
        'B'
      } else if (impossibleToHaveBeacon(data, c)) {
        '#'
      } else {
        ' '
      }
    )
    println(Field2D.toDebugRepresentation(filledOut))
  }

  private def findHiddenBeacon(
    data: Parsed,
    maxCoords: Int,
  ): Option[Coords2D] = {
    val sensorsAt = data.map(_.sensor).toSet
    val beaconsAt = data.map(_.closestBeacon).toSet

    (0 to maxCoords)
      .map { y =>
        if (y % 100000 == 0) {
          println(s"Processing row $y")
        }

        val impossibilityInRow =
          impossibilityIntervalsInRow(data, y, 0, maxCoords)
        val values             = impossibilityInRow.values

        values
          .map { x =>
            Coords2D.of(x, y)
          }
          .find { c =>
            !sensorsAt.contains(c) && !beaconsAt.contains(c)
          }
      }
      .find(_.isDefined)
      .flatten
  }

  private val MagicValue                                = 4000000
  def part2(data: Parsed, maxCoords: Int): Option[Long] =
    findHiddenBeacon(data, maxCoords) map { result =>
      result.x.value.toLong * MagicValue + result.y.value
    }

  def main(args: Array[String]): Unit = {
    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10))
      .intervalWithY(10) shouldEqual Some(
      InclusiveDiscreteInterval(2, 14)
    )

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10))
      .intervalWithY(7) shouldEqual Some(
      InclusiveDiscreteInterval(-1, 17)
    )

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10))
      .intervalWithY(-4) shouldEqual None

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10))
      .intervalWithY(21) shouldEqual None

    val testData = readFileText("2022/15-test.txt")
    val realData = readFileText("2022/15.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test, 10) shouldEqual 26
    part1(real, 2000000) shouldEqual 4861076

    part2(test, 20) shouldEqual Some(56000011)
    part2(real, MagicValue) shouldEqual Some(10649103160102L)
  }
}
