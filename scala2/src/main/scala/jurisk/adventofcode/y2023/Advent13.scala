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
    def covered: Int
    def value: Int
  }

  private object Reflection {
    final case class Horizontal(skipTop: Int, covered: Int) extends Reflection {
      def value: Int = skipTop * 100
    }
    final case class Vertical(skipLeft: Int, covered: Int)  extends Reflection {
      def value: Int = skipLeft
    }
  }

  def parse(input: String): Input =
    input.splitByDoubleNewline.map(s => Field2D.parseBooleanField(s))

  private def verticalReflections(
    field: Field2D[Boolean]
  ): List[Reflection.Vertical] =
    horizontalReflections(field.rotate(Rotation.Left90)).map { reflection =>
      Reflection.Vertical(reflection.skipTop, reflection.covered)
    }

  private def horizontalReflectionPerfect(
    field: Field2D[Boolean]
  ): Option[Reflection.Horizontal] =
    if (field.height % 2 == 0) {
      val half = field.height / 2
      val a    = field.topRows(half)
      val b    = field.bottomRows(half).reverseRows

      (a == b).option(Reflection.Horizontal(half, field.height))
    } else none

  private def horizontalReflections(
    field: Field2D[Boolean]
  ): List[Reflection.Horizontal] = {
    val maxSkip = field.height - 2

    val optionsDropBottom = (0 to maxSkip).map { drop =>
      val newField = field.topRows(field.height - drop)
      horizontalReflectionPerfect(newField)
    }

    // Starts from 1 on purpose so we don't do unchanged "field" twice
    val optionsDropTop = (1 to maxSkip).map { drop =>
      val newField = field.bottomRows(field.height - drop)
      horizontalReflectionPerfect(newField).map(result =>
        result.copy(skipTop = result.skipTop + drop)
      )
    }

    (optionsDropBottom.toList ::: optionsDropTop.toList).flatten.sortBy(x =>
      x.covered
    )(Ordering[Int].reverse)
  }

  private def values(field: Field2D[Boolean]): List[Int] =
    (verticalReflections(field) ::: horizontalReflections(field)).map(_.value)

  def initialValue(field: Field2D[Boolean]): Int =
    values(field).singleResultUnsafe

  private def repairedOptions(field: Field2D[Boolean]): Seq[Field2D[Boolean]] =
    field.allCoords.map { c =>
      field.updatedAtUnsafe(c, !field.at(c).get)
    }

  def repairedValue(field: Field2D[Boolean]): Int = {
    val initial = initialValue(field)

    val results = repairedOptions(field)
      .flatMap(values)
      .filterNot(_ == initial)
      .distinct
      .toList

    results match {
      case x :: Nil => x
      case what     => what.toString.fail
    }
  }

  def part1(data: Input): Int =
    data.map(initialValue).sum

  def part2(data: Input): Int =
    data.map(repairedValue).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/13.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
