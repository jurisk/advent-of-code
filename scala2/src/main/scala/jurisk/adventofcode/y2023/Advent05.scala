package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent05 {
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
    ranges: List[Range],
  ) {
    def convert(n: Long): Long =
      ranges.filter(_.matches(n)) match {
        case range :: Nil => range.convert(n)
        case Nil          => n
        case _            => sys.error(s"Found too many ranges that match $n")
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

          val ranges = t map Range.parse

          ConversionMap(from, to, ranges)

        case _ => input.failedToParse
      }
    }
  }

  final case class Range(
    destinationStart: Long,
    sourceStart: Long,
    length: Long,
  ) {
    private val source = sourceStart until sourceStart + length
    private val diff   = destinationStart - sourceStart

    def matches(n: Long): Boolean = source contains n
    def convert(n: Long): Long    = n + diff
  }

  private object Range {
    def parse(input: String): Range = {
      val List(a, b, c) = input.extractLongs
      Range(a, b, c)
    }
  }

  def parse(input: String): Input = {
    val sections = input.split("\n\n").toList
    sections match {
      case h :: t => Input(h.extractLongs, t map ConversionMap.parse)
      case _      => input.failedToParse
    }
  }

  def solve(data: Input, seeds: List[Seq[Long]]): Long = {
    val total = seeds.map(_.length.toLong).sum
    println(s"Total to process: $total")

    var processed = 0L

    seeds.map { seed =>
      var result = Long.MaxValue

      seed foreach { n =>
        val r = data.seedToLocation(n)
        processed += 1

        if (r < result) {
          println(s"Found $r at $processed")
          result = result.min(r)
        }

        if (processed % 10_000_000 == 0) {
          println(processed)
        }
      }

      result
    }.min
  }

  def part1(data: Input): Long = {
    val seeds = data.seedInput.map(x => x :: Nil)

    solve(data, seeds)
  }

  def part2(data: Input): Long = {
    val seeds = {
      assert(data.seedInput.length % 2 == 0, "Odd count of input for seeds!")
      data.seedInput
        .grouped(2)
        .map { x =>
          val List(from, len) = x
          from until (from + len)
        }
        .toList
    }

    solve(data, seeds)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/05.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
