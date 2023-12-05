package jurisk.adventofcode.y2023

import cats.effect.{IO, IOApp}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import cats.implicits._
import jurisk.math.InclusiveDiscreteInterval

object Advent05 extends IOApp.Simple {
  final case class Input(
    seedInput: List[Long],
    conversionMaps: List[ConversionMap],
  ) {
    def seedToLocation(seed: Long): Long =
      conversionMaps.foldLeft(seed) { case (current, map) =>
        map convert current
      }
  }

  final case class ConversionMap(
    from: String,
    to: String,
    converters: List[Converter],
  ) {
    def convert(n: Long): Long =
      converters.find(_.matches(n)) match {
        case Some(converter) => converter.convert(n)
        case None            => n
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

  final case class Converter(
    destinationStart: Long,
    sourceStart: Long,
    length: Long,
  ) {
    private val diff = destinationStart - sourceStart

    def matches(n: Long): Boolean =
      (n >= sourceStart) && (n < sourceStart + length)
    def convert(n: Long): Long    = n + diff
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
        (seedRange.from to seedRange.to).foldLeft(Long.MaxValue) {
          case (acc, seed) =>
            acc min data.seedToLocation(seed)
        }
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
