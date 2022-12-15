package jurisk.adventofcode.y2022

import jurisk.adventofcode.y2022.Advent15.Interval
import jurisk.geometry.{Area2D, Coords2D, Field2D, X, Y}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent15 {
  type Parsed = List[Entry]

  final case class Entry(sensor: Coords2D, closestBeacon: Coords2D) {
    def impossibleToHaveBeacon(c: Coords2D): Boolean = {
      sensor.manhattanDistance(c) <= sensor.manhattanDistance(closestBeacon)
    }

    def minX: Int = Math.min(sensor.x.value, closestBeacon.x.value)
    def maxX: Int = Math.max(sensor.x.value, closestBeacon.x.value)

    def intervalWithY(y: Int): Option[Interval] = {
      val dy = Math.abs(sensor.y.value - y)
      val dist = sensor.manhattanDistance(closestBeacon)
      if (dist < dy) {
        None
      } else {
        Some(
          Interval(sensor.x.value - (dist - dy), sensor.x.value + (dist - dy))
        )
      }
    }
  }

  object Entry {
    def parse(s: String): Entry =
      s match {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by"            =>
          Entry(Coords2D.of(sx.toInt, sy.toInt), Coords2D.of(bx.toInt, by.toInt))
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Entry.parse)

  private def impossibleToHaveBeacon(data: Parsed, c: Coords2D): Boolean = {
    data.exists(_.impossibleToHaveBeacon(c))
  }

  final case class NonOverlappingIntervalSet(
    data: Set[Interval],
  ) {
    def subtract(interval: Interval): NonOverlappingIntervalSet = NonOverlappingIntervalSet(
      data.flatMap(_.subtract(interval))
    )

    def size: Int = data.map(_.size).sum
    def values: Set[Int] = data.flatMap(_.values)
  }

  object NonOverlappingIntervalSet {
    def create(from: Int, to: Int): NonOverlappingIntervalSet = NonOverlappingIntervalSet(
      Set(Interval(from, to))
    )
  }

  // Inclusive!
  final case class Interval(from: Int, to: Int) {
    def size: Int = to - from + 1
    def values: List[Int] = (from to to).toList
    def subtract(other: Interval): Set[Interval] = {
      val result: Set[Interval] =
        if (other.from > to) {
          Set(this)
        } else if (other.to < from) {
          Set(this)
        } else if ((other.from <= from) && (other.to >= to)) {
          Set.empty
        } else if ((other.from > from) && (other.to < to)) {
          Set(
            Interval(from, other.from - 1),
            Interval(other.to + 1, to),
          )
        } else if ((other.from <= from) && (other.to < to)) {
          Set(
            Interval(other.to + 1, to)
          )
        } else if ((other.from > from) && (other.to >= to)) {
          Set(
            Interval(from, other.from - 1)
          )
        } else if (other.from > to) {
          Set(this)
        } else if (other.to < from) {
          Set(this)
        } else {
          sys.error(s"Unexpected $this subtract $other")
        }

      // println(s"$this - $other = $result")

      result
    }
  }

  private def impossibilityIntervalsInRow(data: Parsed, y: Int, from: Int, to: Int): NonOverlappingIntervalSet = {
    val result = data.foldLeft(NonOverlappingIntervalSet.create(from, to)) { case (acc, e) =>
      val interval = e.intervalWithY(y)
      // println(s"Entry $e interval with $y is $interval")
      interval match {
        case Some(interval) => acc.subtract(interval)
        case None => acc
      }
    }
    // println(s"Row $y from $from to $to: ${result.data} and ${result.values.toList.sorted}")
    result
  }

  def part1(data: Parsed, y: Int): Int = {
    val minX = data.map(_.minX).min * 2
    val maxX = data.map(_.maxX).max * 2
    // println(s"$minX, $maxX")

    require(!impossibleToHaveBeacon(data, Coords2D.of(minX, y)))
    require(!impossibleToHaveBeacon(data, Coords2D.of(maxX, y)))


    val intervalSet = impossibilityIntervalsInRow(data, y, minX, maxX)
    val thisSize = intervalSet.size
    val rangeSize = maxX - minX
    val resultNew = rangeSize - thisSize

    val resultLegacy = (minX to maxX).count { x =>
      val c = Coords2D.of(x, y)
      impossibleToHaveBeacon(data, c)
    }
//
//    require(resultNew == resultLegacy)
//
//    val beaconsHere = data.map(_.closestBeacon.y.value).distinct.count(_ == y)
//
//    resultLegacy - beaconsHere
    resultNew
  }

  def printData(data: Parsed, maxCoords: Int): Unit = {
    val sensorsAt = data.map(_.sensor).toSet
    val beaconsAt = data.map(_.closestBeacon).toSet

    val field = Field2D.ofSize(maxCoords + 1, maxCoords + 1, ' ')
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

  def findHiddenBeacon(data: Parsed, maxCoords: Int): Coords2D = {
    val sensorsAt = data.map(_.sensor).toSet
    val beaconsAt = data.map(_.closestBeacon).toSet

    (0 to maxCoords) foreach { y =>
      if (y % 100000 == 0) {
        println(s"Processing row $y")
      }
      val impossibilityInRow = impossibilityIntervalsInRow(data, y, 0, maxCoords)
      val values = impossibilityInRow.values

      values foreach { x =>
        val c = Coords2D.of(x, y)
        if (sensorsAt.contains(c)) {
        } else if (beaconsAt.contains(c)) {
        } else {
          return c
        }
      }
    }

    sys.error("fase")

  }

  def part2(data: Parsed,  maxCoords: Int): Long = {
    val result = findHiddenBeacon(data, maxCoords)
    result.x.value.toLong * 4000000 + result.y.value
  }


  def main(args: Array[String]): Unit = {
    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10)).intervalWithY(10) shouldEqual Some(
      Interval(2, 14)
    )

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10)).intervalWithY(7) shouldEqual Some(
      Interval(-1, 17)
    )

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10)).intervalWithY(-4) shouldEqual None

    Entry(Coords2D.of(8, 7), Coords2D.of(2, 10)).intervalWithY(21) shouldEqual None

    // https://i.stack.imgur.com/h2Nw2.png
    Interval(11, 30) subtract Interval(36, 41) shouldEqual Set(Interval(11, 30))
    Interval(11, 30) subtract Interval(5, 20) shouldEqual Set(Interval(21, 30))
    Interval(11, 30) subtract Interval(30, 33) shouldEqual Set(Interval(11, 29))
    Interval(11, 30) subtract Interval(8, 35) shouldEqual Set.empty
    Interval(11, 30) subtract Interval(3, 8) shouldEqual Set(Interval(11, 30))
    Interval(11, 30) subtract Interval(18, 27) shouldEqual Set(
      Interval(11, 17),
      Interval(28, 30),
    )

    val testData = readFileText("2022/15-test.txt")
    // val testData = """""";
    val realData = readFileText("2022/15.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test, 10) shouldEqual 26
    part1(real, 2000000) shouldEqual 4861076

    part2(test, 20) shouldEqual 56000011
    part2(real, 4000000) shouldEqual 10649103160102L
  }
}
