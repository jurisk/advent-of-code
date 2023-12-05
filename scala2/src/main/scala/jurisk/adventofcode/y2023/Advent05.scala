package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.NumericRange

object Advent05 {
  val Seed        = "seed"
  val Soil        = "soil"
  val Fertilizer  = "fertilizer"
  val Water       = "water"
  val Light       = "light"
  val Temperature = "temperature"
  val Humidity    = "humidity"
  val Location    = "location"

  val Order =
    Seed :: Soil :: Fertilizer :: Water :: Light :: Temperature :: Humidity :: Location :: Nil

  final case class Input(
    seedInput: List[Long],
    conversionMaps: List[ConversionMap],
  ) {
    def seedsPart1: List[Long]                         = seedInput
    def seedsPart2: List[NumericRange.Exclusive[Long]] = {
      assert(seedInput.length % 2 == 0)

      println(seedInput.grouped(2).map(_(1)).sum)

      seedInput.grouped(2).toList.map { x =>
        val List(from, len) = x
        from until (from + len)
      }
    }

    def seedToLocation(seed: Long): Long = {

      var current = seed
      conversionMaps foreach { map =>
        current = map.convert(current)
      }
      current
    }

    def seedToLocationOld(seed: Long): Long = {
      val pairs: List[(String, String)] = Order.init zip Order.tail

      var current = seed
      pairs foreach { case (from, to) =>
        val newCurrent = convertOld(current, from, to)
//        println(s"Converting $current using $from to $to: got $newCurrent")
        current = newCurrent
      }

      current
    }

    def convertOld(n: Long, from: String, to: String): Long = {
      val conversionMap =
        conversionMaps.filter(m => m.from == from && m.to == to).head
      conversionMap.convert(n)
    }
  }

  final case class ConversionMap(
    from: String,
    to: String,
    ranges: List[Range],
  ) {
    def convert(n: Long): Long =
      ranges.find(range => range.matches(n)) match {
        case Some(range) => range.convert(n)
        case _           => n
      }
  }

  object ConversionMap {
    def parse(input: String): ConversionMap = {
      val lines = input.split("\n").toList
      lines match {
        case h :: t =>
          val (from, to) = h match {
            case s"$from-to-$to map:" => (from, to)
            case _                    => h.failedToParse
          }

          val ranges = t.map(Range.parse)

          ConversionMap(from, to, ranges)
        case _      => input.failedToParse
      }
    }
  }

  final case class Range(
    destinationStart: Long,
    sourceStart: Long,
    length: Long,
  ) {
    def matches(n: Long): Boolean =
      (n >= sourceStart) && (n < sourceStart + length)

    def convert(n: Long): Long =
      n + (destinationStart - sourceStart)
  }

  object Range {
    def parse(input: String): Range = {
      val List(a, b, c) = input.extractLongs
      Range(a, b, c)
    }
  }

  def parse(input: String): Input = {
    val sections = input.split("\n\n").toList
    sections match {
      case h :: t =>
        val seeds          = h.extractLongs
        val conversionMaps = t.map(ConversionMap.parse)

        Input(
          seeds,
          conversionMaps,
        )

      case _ => input.failedToParse
    }
  }

  def part1(data: Input): Long =
    data.seedsPart1.map(x => data.seedToLocation(x)).min

  def part2(data: Input): Long = {
    var result    = Long.MaxValue
    var processed = 0L

    data.seedsPart2.foreach { x =>
      x.foreach { n =>
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
    }

    result
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/05.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
