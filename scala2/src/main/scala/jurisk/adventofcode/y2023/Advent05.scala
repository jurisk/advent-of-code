package jurisk.adventofcode.y2023

import cats.effect.{IO, IOApp}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import cats.implicits._
import jurisk.math.{DiscreteInterval, DiscreteIntervalSet}

object Advent05 extends IOApp.Simple {
  final case class Input(
    seedInput: List[Long],
    conversionMaps: List[ConversionMap],
  ) {
    def seedToLocationPotentialIntervals(
      interval: DiscreteInterval[Long]
    ): DiscreteIntervalSet[Long] = {
      val startSet = DiscreteIntervalSet.continuous(interval)

      conversionMaps.foldLeft(startSet) { case (current, map) =>
        map potentialIntervals current
      }
    }
  }

  final case class ConversionMap(
    from: String,
    to: String,
    converters: List[Converter],
  ) {
    def potentialIntervals(
      intervalSet: DiscreteIntervalSet[Long]
    ): DiscreteIntervalSet[Long] = {
      val results = converters.foldLeft(ConversionResult.empty) {
        case (acc, converter) =>
          acc union converter.potentialIntervals(intervalSet)
      }

      // The converters did not pick up some intervals, they stay as they were
      val straightThrough = intervalSet subtract results.covered

      straightThrough union results.converted
    }
  }

  private object ConversionMap {
    // Note - the conversion maps must be in the right order in the input!
    def parse(input: String): ConversionMap = {
      val lines = input.split("\n").toList
      lines match {
        case h :: t =>
          val (from, to) = h match {
            case s"$from-to-$to map:" => (from, to)
            case _                    => h.failedToParse
          }

          val ranges = t map Converter.parse

          ConversionMap(from, to, ranges)

        case _ => input.failedToParse
      }
    }
  }

  final case class ConversionResult(
    uncovered: DiscreteIntervalSet[Long],
    covered: DiscreteIntervalSet[Long],
    converted: DiscreteIntervalSet[Long],
  ) {
    def union(other: ConversionResult): ConversionResult =
      ConversionResult(
        uncovered = uncovered union other.uncovered,
        covered = covered union other.covered,
        converted = converted union other.converted,
      )
  }

  object ConversionResult {
    def empty: ConversionResult = ConversionResult(
      DiscreteIntervalSet.empty[Long],
      DiscreteIntervalSet.empty[Long],
      DiscreteIntervalSet.empty[Long],
    )
  }

  final case class Converter(
    destinationStart: Long,
    sourceStart: Long,
    length: Long,
  ) {
    override def toString: String = s"$sourceInterval -> $diff"

    private val diff           = destinationStart - sourceStart
    private val sourceInterval =
      DiscreteInterval(sourceStart, sourceStart + length - 1)

    private def convert(n: Long): Long = n + diff

    def potentialIntervals(
      intervalSet: DiscreteIntervalSet[Long]
    ): ConversionResult =
      intervalSet.intervals.foldLeft(ConversionResult.empty) {
        case (acc, interval) =>
          acc union tryConvert(interval)
      }

    private def tryConvert(
      interval: DiscreteInterval[Long]
    ): ConversionResult = {
      val relevant = interval intersect sourceInterval

      relevant match {
        case Some(covered) =>
          val uncovered = interval subtract covered
          val converted =
            DiscreteInterval(
              convert(covered.from),
              convert(covered.to),
            )
          ConversionResult(
            uncovered = uncovered,
            covered = DiscreteIntervalSet.continuous(covered),
            converted = DiscreteIntervalSet.continuous(converted),
          )
        case None          =>
          ConversionResult(
            uncovered = DiscreteIntervalSet.continuous(interval),
            covered = DiscreteIntervalSet.empty[Long],
            converted = DiscreteIntervalSet.empty[Long],
          )

      }
    }
  }

  private object Converter {
    def parse(input: String): Converter = {
      val List(a, b, c) = input.extractLongList
      Converter(a, b, c)
    }
  }

  def parse(input: String): Input = {
    val sections = input.split("\n\n").toList
    sections match {
      case h :: t => Input(h.extractLongList, t map ConversionMap.parse)
      case _      => input.failedToParse
    }
  }

  def solve(
    data: Input,
    seedRanges: List[DiscreteInterval[Long]],
  ): IO[Long] = {
    def minForSeedRange(seedRange: DiscreteInterval[Long]): IO[Long] =
      IO {
        data
          .seedToLocationPotentialIntervals(seedRange)
          .minOption getOrElse Long.MaxValue
      }

    val total = seedRanges.map(_.size).sum

    for {
      _       <- IO.println(s"Total to process: $total")
      results <- seedRanges.zipWithIndex
                   .parTraverse { case (seedRange, idx) =>
                     IO.println(
                       s"Processing seed range $idx: $seedRange..."
                     ) *> minForSeedRange(seedRange)
                   }
    } yield results.min
  }

  def part1(data: Input): IO[Long] = {
    val seeds = data.seedInput.map(x => DiscreteInterval(x, x))

    solve(data, seeds)
  }

  def part2(data: Input): IO[Long] = {
    val seeds = {
      assert(data.seedInput.length % 2 == 0, "Odd count of input for seeds!")
      data.seedInput
        .grouped(2)
        .map { x =>
          val List(from, len) = x
          DiscreteInterval(from, from + len - 1)
        }
        .toList
    }

    solve(data, seeds)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  override def run: IO[Unit] = for {
    realData <- IO(parseFile("2023/05.txt"))

    result1 <- part1(realData)
    _       <- IO.println(s"Part 1: $result1")
    result2 <- part2(realData)
    _       <- IO.println(s"Part 2: $result2")
  } yield ()
}
