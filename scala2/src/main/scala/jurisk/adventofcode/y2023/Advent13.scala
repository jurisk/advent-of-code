package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.geometry.Field2D
import jurisk.geometry.Field2D.printBooleanField
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent13 {
  type Input = List[Field2D[Boolean]]

  def parse(input: String): Input = {
    val v = input.splitByDoubleNewline
    v.map(x => Field2D.parseBooleanField(x))
  }

  def verticalReflection(field: Field2D[Boolean]): Option[Int] = {
    val result = horizontalReflectionInt(field.rotate90Left)

    println(s"Vert $result:")
    printBooleanField(field)

    result
  }

  def horizontalReflection(field: Field2D[Boolean]): Option[Int] = {
    val result = horizontalReflectionInt(field)

    println(s"Horiz $result:")
    printBooleanField(field)

    result
  }

  def horizontalReflectionPerfect(field: Field2D[Boolean]): Option[Int] =
    if (field.height % 2 == 0) {
      val half = field.height / 2
      val a    = Field2D(field.data.take(half))
      val b    = Field2D(field.data.drop(half))

      val bReverse = Field2D(b.data.reverse)

      if (a == bReverse) {
        half.some
      } else {
        none
      }
    } else none

  def horizontalReflectionInt(field: Field2D[Boolean]): Option[Int] = {
    val optionsDropTop = (0 to field.height - 2).map { drop =>
      val newField = Field2D(field.data.drop(drop))
      horizontalReflectionPerfect(newField).map(_ + drop)
    }

    val optionsDropBottom = (0 to field.height - 2).map { drop =>
      val newField = Field2D(field.data.take(field.height - drop))
      horizontalReflectionPerfect(newField)
    }

    (optionsDropTop.toList ::: optionsDropBottom.toList).flatten match {
      case x :: Nil => x.some
      case Nil      => None
      case what     => what.toString.fail
    }
  }

  def value(field: Field2D[Boolean]): Int = {
    val v = verticalReflection(field) getOrElse 0                     // .map(_ + 1)
    val h = horizontalReflection(field).map(x => x * 100) getOrElse 0 // (x + 1)

    if (v == 0 && h == 0) {
      printBooleanField(field)
      "fail".fail
    }

    v + h
  }

  def part1(data: Input): Int =
    data.map(value).sum

  def part2(data: Input): Int =
    ???

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/13.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
