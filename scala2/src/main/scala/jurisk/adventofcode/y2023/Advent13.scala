package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.geometry.{Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent13 {
  type Input = List[Field2D[Boolean]]

  def parse(input: String): Input = {
    val v = input.splitByDoubleNewline
    v.map(x => Field2D.parseBooleanField(x))
  }

  def verticalReflection(field: Field2D[Boolean]): Option[Int] =
    verticalReflectionInt(field).headOption

  def verticalReflectionInt(field: Field2D[Boolean]): List[Int] =
    horizontalReflectionInt(field.rotate(Rotation.Left90))

  def horizontalReflection(field: Field2D[Boolean]): Option[Int] =
    horizontalReflectionInt(field).headOption

  def horizontalReflectionPerfect(field: Field2D[Boolean]): Option[Int] =
    if (field.height % 2 == 0) {
      val half = field.height / 2
      val a    = field.topRows(half)
      val b    = field.bottomRows(half).reverseRows

      if (a == b) {
        half.some
      } else {
        none
      }
    } else none

  def horizontalReflectionInt(field: Field2D[Boolean]): List[Int] = {
    val maxSkip = field.height - 2

    val optionsDropBottom = (0 to maxSkip).map { drop =>
      val newField = field.topRows(field.height - drop)
      horizontalReflectionPerfect(newField)
    }

    val optionsDropTop = (0 to maxSkip).map { drop =>
      val newField = field.bottomRows(field.height - drop)
      horizontalReflectionPerfect(newField).map(_ + drop)
    }

    (optionsDropBottom.toList ::: optionsDropTop.toList).flatten.sortBy(x =>
      (x - field.height / 2).abs
    )
  }

  def valueInt(field: Field2D[Boolean]): List[Int] = {
    val v = verticalReflection(field)
    val h = horizontalReflection(field).map(x => x * 100)

    v.toList ::: h.toList
  }

  def valuesInt(field: Field2D[Boolean]): List[Int] = {
    val v = verticalReflectionInt(field)
    val h = horizontalReflectionInt(field).map(x => x * 100)

    v ::: h
  }

  def value(field: Field2D[Boolean]): Int =
    valueInt(field).singleResultUnsafe

  def fixor(field: Field2D[Boolean]): Seq[Field2D[Boolean]] =
    field.allCoords.map { c =>
      field.updatedAtUnsafe(c, !field.at(c).get)
    }

  def fixedValue(field: Field2D[Boolean]): Int = {
    val originalValue = value(field)

    val results = fixor(field)
      .flatMap(valuesInt)
      .filterNot(_ == originalValue)
      .distinct
      .toList

    results match {
      case x :: Nil => x
      case what     => what.toString.fail
    }
  }

  def part1(data: Input): Int =
    data.map(value).sum

  def part2(data: Input): Int =
    data.map(fixedValue).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/13.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
