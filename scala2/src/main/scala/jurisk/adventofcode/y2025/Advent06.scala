package jurisk.adventofcode.y2025

import cats.kernel.Monoid
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

object Advent06 {
  type Input = (ArraySeq[String], ArraySeq[Operator])
  type N     = Long

  sealed trait Operator extends Product with Serializable {
    def monoid: Monoid[N]
  }
  object Operator {
    case object Add      extends Operator {
      override def monoid: Monoid[N] = new Monoid[N] {
        override def empty: N               = 0
        override def combine(x: N, y: N): N = x + y
      }
    }
    case object Multiply extends Operator {
      override def monoid: Monoid[N] = new Monoid[N] {
        override def empty: N               = 1
        override def combine(x: N, y: N): N = x * y
      }
    }

    def parse(s: String): Operator =
      s match {
        case "*" => Multiply
        case "+" => Add
        case _   => s.failedToParse
      }
  }

  def parse(input: String): Input = {
    val lines = input.splitLines
    val a     = ArraySeq.from(lines.init)
    val b     = ArraySeq.from(
      lines.last.split(" ").map(_.trim).filter(_.nonEmpty).map(Operator.parse)
    )
    (a, b)
  }

  def part1(data: Input): N = {
    val (numberStrings, operators) = data
    val numbers                    = numberStrings.map(_.extractLongArraySeq)
    operators.zipWithIndex.map { case (operator, idx) =>
      operator.monoid.combineAll(numbers.map(_(idx)))
    }.sum
  }

  def part2(data: Input): N = {
    val (numberStrings, operators) = data

    assert(numberStrings.map(_.length).distinct.size == 1)

    val transposed = (0 until numberStrings.head.length)
      .map { idx =>
        numberStrings.map(_.charAt(idx))
      }
      .map(_.mkString)

    val tempNumbers = transposed.map { s =>
      s.trim.toLongOption
    }

    val NumberSeparator = '_'
    val GroupSeparator  = '|'
    val hack            = tempNumbers
      .map {
        case Some(value) => value.toString
        case None        => GroupSeparator
      }
      .mkString(NumberSeparator.toString)

    val groups = ArraySeq.from(hack.split(GroupSeparator))

    val numbers = groups.map(n =>
      ArraySeq.from(n.split(NumberSeparator).flatMap(_.toLongOption))
    )

    assert(numbers.length == operators.length)

    numbers
      .zip(operators)
      .map { case (group, operator) =>
        operator.monoid.combineAll(group)
      }
      .sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/06$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
