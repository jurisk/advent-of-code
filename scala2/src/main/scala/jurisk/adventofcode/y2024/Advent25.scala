package jurisk.adventofcode.y2024

import cats.implicits.{catsSyntaxEitherId, toFoldableOps}
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent25 {
  type Input = List[Field2D[Boolean]]
  type N     = Long

  def parse(input: String): Input =
    input.parseSections(s => Field2D.parseBooleanField(s))

  sealed trait Schematic
  object Schematic {
    final case class Lock(heights: List[Int]) extends Schematic
    final case class Key(heights: List[Int])  extends Schematic

    def convert(field: Field2D[Boolean]): Either[Lock, Key] =
      if (field.firstRowValues.forall(_ == true)) {
        Lock(field.columns.map(_.count(identity) - 1)).asLeft
      } else if (field.firstRowValues.forall(_ == false)) {
        Key(field.columns.map(_.count(identity) - 1)).asRight
      } else {
        "Invalid field".fail
      }

    def fits(lock: Lock, key: Key): Boolean = {
      val Height = 7
      lock.heights.zip(key.heights).forall { case (l, k) =>
        l + k <= Height - 2
      }
    }
  }

  def part1(data: Input): N = {
    val (locks, keys) = data.map(Schematic.convert).partitionEither(identity)
    (for {
      lock <- locks
      key  <- keys
    } yield Schematic.fits(lock, key)).count(identity)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/25$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
  }
}
