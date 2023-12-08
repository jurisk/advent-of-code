package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.adventofcode.y2018.Advent04.Entry.BeginsShift
import jurisk.utils.FileInput.parseFileLines
import jurisk.utils.Parsing.splitIntoSections
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

import java.time.Duration
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object Advent04 {
  final case class Timestamp(
    private val dateTime: LocalDateTime
  ) {
    def minutesTo(other: Timestamp): Long =
      Duration.between(dateTime, other.dateTime).toMinutes

    def isBefore(other: Timestamp): Boolean = dateTime.isBefore(other.dateTime)
    def minute: Int                         = dateTime.getMinute
    def addMinutes(minutes: Int): Timestamp = Timestamp(
      dateTime.plus(minutes, ChronoUnit.MINUTES)
    )
  }

  object Timestamp {
    implicit val ordering: Ordering[Timestamp] =
      Ordering[LocalDateTime].contramap(_.dateTime)
  }

  type GuardId = Int

  sealed trait Entry {
    def timestamp: Timestamp
  }

  object Entry {
    final case class BeginsShift(
      guardId: GuardId,
      timestamp: Timestamp,
    ) extends Entry

    final case class WakesUp(
      timestamp: Timestamp
    ) extends Entry

    final case class FallsAsleep(
      timestamp: Timestamp
    ) extends Entry

    private val RegEx           = """\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)] (.+)""".r
    private val RegEx2          = """Guard #(\d+) begins shift""".r
    def parse(s: String): Entry =
      s match {
        case RegEx(year, month, day, hours, minutes, rest) =>
          val timestamp = Timestamp(
            LocalDateTime.of(
              year.toInt,
              month.toInt,
              day.toInt,
              hours.toInt,
              minutes.toInt,
            )
          )

          rest match {
            case "falls asleep"  => FallsAsleep(timestamp)
            case "wakes up"      => WakesUp(timestamp)
            case RegEx2(guardId) => BeginsShift(guardId.toInt, timestamp)
            case _               => sys.error(s"Failed to parse $s")
          }
        case _                                             => sys.error(s"Failed to parse $s")
      }
  }

  final case class SleepInterval(from: Timestamp, to: Timestamp) {
    def durationMinutes: Long = from.minutesTo(to)

    def containedMinutes: List[Timestamp] = {
      val result = List.unfold(from) { current =>
        if (current.isBefore(to)) {
          (
            current,
            current.addMinutes(1),
          ).some
        } else {
          none
        }
      }

      result
    }
  }

  def readFileAndParse(fileName: String): List[Entry] =
    parseFileLines(fileName, Entry.parse)

  private def segment(data: List[Entry]): List[(Entry, List[Entry])] =
    splitIntoSections[Entry](
      data,
      {
        case BeginsShift(_, _) => true
        case _                 => false
      },
    )

  private def aggregate(
    data: List[Entry]
  ): Map[GuardId, List[SleepInterval]] = {
    val sorted = data.sortBy(_.timestamp)

    val segmented = segment(sorted)

    val info = segmented.map {
      case (BeginsShift(guardId, _), tail) =>
        require(tail.length % 2 == 0)
        guardId            -> tail
          .grouped(2)
          .map(group => SleepInterval(group.head.timestamp, group(1).timestamp))
          .toList
      case list                            => sys.error(s"Unexpected $list")
    }

    info.groupMapReduce { case (k, _) => k } { case (_, v) => v } {
      case (at, bt) => at ++ bt
    }
  }

  private def minuteMostAsleep(intervals: List[SleepInterval]): Int = {
    val (minute, _) = intervals
      .flatMap(_.containedMinutes)
      .map(_.minute)
      .counts
      .maxBy { case (_, v) => v }

    minute
  }

  def part1(data: List[Entry]): Long = {
    val aggregated = aggregate(data)

    val (sleepiestGuardId, _) = aggregated
      .maxBy { case (_, intervals) =>
        intervals.map(_.durationMinutes).sum
      }

    val sleepiestMinute = minuteMostAsleep(aggregated(sleepiestGuardId))

    sleepiestGuardId * sleepiestMinute
  }

  def part2(data: List[Entry]): Long = {
    val aggregated = aggregate(data)

    val guardMinutes: List[(GuardId, Int, Int)] = aggregated.toList flatMap {
      case (_, Nil) =>
        none

      case (guard, intervals) =>
        val minutes         = intervals.flatMap(_.containedMinutes.map(_.minute))
        val (minute, count) = minutes.counts.maxBy { case (_, count) => count }
        (guard, minute, count).some
    }

    val (guard, minute, _) = guardMinutes.maxBy { case (_, _, count) => count }

    guard * minute
  }

  def main(args: Array[String]): Unit = {
    val real = readFileAndParse("2018/04.txt")
    val test = readFileAndParse("2018/04-test.txt")

    part1(test) shouldEqual 240
    part1(real) shouldEqual 3212

    part2(test) shouldEqual 4455
    part2(real) shouldEqual 4966
  }
}
