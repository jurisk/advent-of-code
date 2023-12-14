package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.geometry.{Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import mouse.all._

object Advent13 {
  type Input = List[Field2D[Boolean]]

  sealed trait Reflection {
    def value: Int
  }

  object Reflection {
    final case class Horizontal(skipTop: Int) extends Reflection {
      def value: Int = skipTop * 100
    }
    final case class Vertical(skipLeft: Int)  extends Reflection {
      def value: Int = skipLeft
    }
  }

  def parse(input: String): Input =
    input.splitByDoubleNewline.map(s => Field2D.parseBooleanField(s))

  private def verticalReflections(
    field: Field2D[Boolean]
  ): List[Reflection.Vertical] =
    horizontalReflections(field.rotate(Rotation.Right90)).map { reflection =>
      Reflection.Vertical(skipLeft = reflection.skipTop)
    }

  private def horizontalReflectionPerfect(
    field: Field2D[Boolean]
  ): Option[Reflection.Horizontal] =
    if (field.height % 2 == 0) {
      val halfHeight         = field.height / 2
      val topHalf            = field.topRows(halfHeight)
      val bottomHalfReversed = field.bottomRows(halfHeight).reverseRows

      (topHalf == bottomHalfReversed).option(Reflection.Horizontal(halfHeight))
    } else none

  private def horizontalReflections(
    field: Field2D[Boolean]
  ): List[Reflection.Horizontal] = {
    val maxDrop = field.height - 2

    val optionsDropBottom = (0 to maxDrop) flatMap { drop =>
      val newField = field.topRows(field.height - drop)
      horizontalReflectionPerfect(newField)
    }

    // Starts from 1 on purpose so we don't do unchanged "field" twice
    val optionsDropTop = (1 to maxDrop) flatMap { drop =>
      val newField = field.bottomRows(field.height - drop)
      horizontalReflectionPerfect(newField) map { result =>
        Reflection.Horizontal(result.skipTop + drop)
      }
    }

    optionsDropBottom.toList ::: optionsDropTop.toList
  }

  private[y2023] def reflections(field: Field2D[Boolean]): List[Reflection] =
    verticalReflections(field) ::: horizontalReflections(field)

  def singleReflection(field: Field2D[Boolean]): Reflection =
    reflections(field).singleResultUnsafe

  private def repairedOptions(field: Field2D[Boolean]): Seq[Field2D[Boolean]] =
    field.allCoords.map { c =>
      field.modifyUnsafe(c, !_)
    }

  def repairedReflection(field: Field2D[Boolean]): Reflection = {
    val initial = singleReflection(field)

    val results = repairedOptions(field)
      .flatMap(reflections)
      .filterNot(_ == initial)
      .toSet

    results.singleResultUnsafe
  }

  def part1(data: Input): Int =
    data.map(singleReflection(_).value).sum

  def part2(data: Input): Int =
    data.map(repairedReflection(_).value).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/13.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
