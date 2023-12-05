package jurisk.adventofcode.y2023

import cats.effect.{IO, IOApp}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import cats.implicits._
import jurisk.math.{
  InclusiveDiscreteInterval,
  NonOverlappingDiscreteIntervalSet,
}

object Advent05 extends IOApp.Simple {
  final case class Input(
    seedInput: List[Long],
    conversionMaps: List[ConversionMap],
  ) {
    def seedToLocationPotentialIntervals(
      interval: InclusiveDiscreteInterval[Long]
    ): NonOverlappingDiscreteIntervalSet[Long] = {
      val startSet = NonOverlappingDiscreteIntervalSet(Set(interval))
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
      intervalSet: NonOverlappingDiscreteIntervalSet[Long]
    ): NonOverlappingDiscreteIntervalSet[Long] = {
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
    uncovered: NonOverlappingDiscreteIntervalSet[Long],
    covered: NonOverlappingDiscreteIntervalSet[Long],
    converted: NonOverlappingDiscreteIntervalSet[Long],
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
      NonOverlappingDiscreteIntervalSet.empty[Long],
      NonOverlappingDiscreteIntervalSet.empty[Long],
      NonOverlappingDiscreteIntervalSet.empty[Long],
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
      InclusiveDiscreteInterval(sourceStart, sourceStart + length - 1)

    private def convert(n: Long): Long = n + diff

    def potentialIntervals(
      intervalSet: NonOverlappingDiscreteIntervalSet[Long]
    ): ConversionResult =
      intervalSet.data.foldLeft(ConversionResult.empty) {
        case (acc, interval) =>
          acc union tryConvert(interval)
      }

    private def tryConvert(
      interval: InclusiveDiscreteInterval[Long]
    ): ConversionResult = {
      val relevant = interval intersect sourceInterval

      relevant match {
        case Some(covered) =>
          val uncovered = interval subtract covered
          val converted =
            InclusiveDiscreteInterval(
              convert(covered.from),
              convert(covered.to),
            )
          ConversionResult(
            uncovered = uncovered,
            covered = NonOverlappingDiscreteIntervalSet(Set(covered)),
            converted = NonOverlappingDiscreteIntervalSet(Set(converted)),
          )
        case None          =>
          ConversionResult(
            uncovered = NonOverlappingDiscreteIntervalSet(Set(interval)),
            covered = NonOverlappingDiscreteIntervalSet.empty[Long],
            converted = NonOverlappingDiscreteIntervalSet.empty[Long],
          )

      }
    }
  }

  private object Converter {
    def parse(input: String): Converter = {
      val List(a, b, c) = input.extractLongs
      Converter(a, b, c)
    }
  }

  def parse(input: String): Input = {
    val sections = input.split("\n\n").toList
    sections match {
      case h :: t => Input(h.extractLongs, t map ConversionMap.parse)
      case _      => input.failedToParse
    }
  }

  def solve(
    data: Input,
    seedRanges: List[InclusiveDiscreteInterval[Long]],
  ): IO[Long] = {
    def minForSeedRange(seedRange: InclusiveDiscreteInterval[Long]): IO[Long] =
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
    val seeds = data.seedInput.map(x => InclusiveDiscreteInterval(x, x))

    solve(data, seeds)
  }

  def part2(data: Input): IO[Long] = {
    val seeds = {
      assert(data.seedInput.length % 2 == 0, "Odd count of input for seeds!")
      data.seedInput
        .grouped(2)
        .map { x =>
          val List(from, len) = x
          InclusiveDiscreteInterval(from, from + len - 1)
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
