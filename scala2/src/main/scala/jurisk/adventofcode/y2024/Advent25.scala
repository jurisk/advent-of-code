package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxEitherId
import cats.implicits.toFoldableOps
import jurisk.geometry.Field2D
import jurisk.utils.CollectionOps.BooleanIterableOnceOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent25 {
  type Input = List[Field2D[Boolean]]
  type N     = Long

  def parse(input: String): Input =
    input.parseSections(s => Field2D.parseBooleanField(s))

  sealed trait Schematic
  private object Schematic {
    final case class Lock(heights: List[Int]) extends Schematic
    final case class Key(heights: List[Int])  extends Schematic {
      def fits(lock: Lock): Boolean = {
        val Height = 7

        lock.heights.zip(heights).forall { case (l, k) =>
          l + k <= Height - 2
        }
      }
    }

    def convert(field: Field2D[Boolean]): Either[Lock, Key] = {
      val heights  = field.columns.map(_.countTrues - 1)
      val firstRow = field.firstRowValues
      val lastRow  = field.lastRowValues
      (
        firstRow.forall(_ == true) && lastRow.forall(_ == false),
        firstRow.forall(_ == false) && lastRow.forall(_ == true),
      ) match {
        case (true, false) =>
          Lock(heights).asLeft
        case (false, true) =>
          Key(heights).asRight
        case _             => "Invalid field".fail
      }
    }
  }

  def part1(data: Input): N = {
    val (locks, keys) = (data map Schematic.convert).partitionEither(identity)
    (for {
      lock <- locks
      key  <- keys
    } yield key fits lock).countTrues
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
