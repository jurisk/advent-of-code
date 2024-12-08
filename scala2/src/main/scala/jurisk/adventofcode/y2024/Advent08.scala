package jurisk.adventofcode.y2024

import cats.implicits._
import jurisk.adventofcode.y2024.Advent08.Square.Antenna
import jurisk.adventofcode.y2024.Advent08.Square.Empty
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import mouse.all._

import scala.annotation.tailrec

object Advent08 {
  private type Frequency = Char

  sealed trait Square
  object Square {
    case object Empty                              extends Square
    final case class Antenna(frequency: Frequency) extends Square

    def parse(ch: Char): Square = ch match {
      case '.' => Empty
      case ch  => Antenna(ch)
    }
  }

  private type SquareAndAntiNodes = (Square, Set[Frequency])
  type Input                      = Field2D[SquareAndAntiNodes]
  type N                          = Long

  def parse(input: String): Input =
    Field2D.parseCharField(input).map(ch => (Square.parse(ch), Set.empty))

  @tailrec
  private def helper(
    field: Input,
    frequency: Char,
    current: Coords2D,
    diff: Coords2D,
    limit: Option[Int],
    count: Int = 0,
  ): Input =
    if (field.isValidCoordinate(current) && limit.forall(count <= _)) {
      val updated = if (limit.isEmpty || count != 0) {
        field.modifyIgnoringInvalidCoords(
          current,
          { case (ch, set) =>
            (ch, set + frequency)
          },
        )
      } else {
        field
      }

      helper(updated, frequency, current + diff, diff, limit, count + 1)
    } else {
      field
    }

  private def update(
    field: Input,
    frequency: Char,
    a: Coords2D,
    b: Coords2D,
    limit: Option[Int],
  ): Input =
    List((a, a - b), (b, b - a)).foldLeft(field) {
      case (field, (start, diff)) =>
        helper(field, frequency, start, diff, limit)
    }

  private def processFrequency(
    field: Input,
    frequency: Char,
    limit: Option[Int],
  ): Input = {
    val locations = field.filterCoordsByValue { case (sq, _) =>
      sq == Antenna(frequency)
    }
    val pairs     = (locations, locations).mapN { case (a, b) =>
      (a != b).option((a, b))
    }.flatten

    pairs.foldLeft(field) { case (field, (a, b)) =>
      update(field, frequency, a, b, limit)
    }
  }

  def solve(data: Input, limit: Option[Int]): N = {
    val frequencies = data.values.flatMap { case (sq, _) =>
      sq match {
        case Antenna(frequency) => Some(frequency)
        case Empty              => None
      }
    }.toSet

    val result = frequencies.foldLeft(data) { case (field, frequency) =>
      processFrequency(field, frequency, limit)
    }

    result.count { case (_, s) => s.nonEmpty }
  }

  def part1(data: Input): N =
    solve(data, 1.some)

  def part2(data: Input): N =
    solve(data, none)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/08$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
