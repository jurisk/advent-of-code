package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._
import jurisk.adventofcode.y2022.Advent21.Operation._
import jurisk.adventofcode.y2022.Advent21.Expression._

import scala.annotation.tailrec

object Advent21 {
  type Parsed = List[(Name, Monkey)]
  type Name   = String
  type Value  = Long

  sealed trait Monkey

  object Monkey {
    case class BinaryMonkey(a: Name, operation: Operation, b: Name)
        extends Monkey
    final case class Literal(value: Value) extends Monkey

    def parse(s: String): (Name, Monkey) =
      s match {
        case s"$name: $a + $b"  => name -> BinaryMonkey(a, Plus, b)
        case s"$name: $a - $b"  => name -> BinaryMonkey(a, Minus, b)
        case s"$name: $a * $b"  => name -> BinaryMonkey(a, Multiply, b)
        case s"$name: $a / $b"  => name -> BinaryMonkey(a, Divide, b)
        case s"$name: $literal" => name -> Literal(literal.toLong)
        case _                  => s"Failed to parse $s".fail
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Monkey.parse)

  sealed trait Operation {
    def execute(a: Value, b: Value): Value
    def toChar: Char
  }

  object Operation {
    case object Plus extends Operation {
      override def execute(a: Value, b: Value): Value = a + b
      override def toChar: Char                       = '+'
    }

    case object Minus extends Operation {
      override def execute(a: Value, b: Value): Value = a - b
      override def toChar: Char                       = '-'
    }

    case object Multiply extends Operation {
      override def execute(a: Value, b: Value): Value = a * b
      override def toChar: Char                       = '*'
    }

    case object Divide extends Operation {
      override def execute(a: Value, b: Value): Value = a / b
      override def toChar: Char                       = '/'
    }
  }

  sealed trait Expression {
    def rearrange: Expression
    def display: String
    def evaluate(unknownX: => Value): Value

    def +(other: Expression): Expression = BinaryExpression(this, Plus, other)
    def *(other: Expression): Expression =
      BinaryExpression(this, Multiply, other)
    def -(other: Expression): Expression = BinaryExpression(this, Minus, other)
    def /(other: Expression): Expression = BinaryExpression(this, Divide, other)
  }

  object Expression {
    case object UnknownX extends Expression {
      override def rearrange: Expression               = this
      override def display: String                     = "x"
      override def evaluate(unknownX: => Value): Value = unknownX
    }

    case class BinaryExpression(
      a: Expression,
      operation: Operation,
      b: Expression,
    ) extends Expression {
      override def rearrange: Expression =
        (a.rearrange, operation, b.rearrange) match {
          case (Literal(a), operation, Literal(b)) =>
            Literal(operation.execute(a, b))

          // (a - x) == (x - a) * -1
          case (Literal(a), Minus, b)              =>
            (b - Literal(a)) * Literal(-1)

          // a * x == x * a
          case (Literal(a), Multiply, b)           =>
            b * Literal(a)

          // a + x == x + a
          case (Literal(a), Plus, b)               =>
            b + Literal(a)

          case (a, _, b) =>
            BinaryExpression(a, operation, b)
        }

      override def display: String =
        s"(${a.display} ${operation.toChar} ${b.display})"

      override def evaluate(unknownX: => Value): Value =
        operation.execute(a.evaluate(unknownX), b.evaluate(unknownX))
    }

    case class Literal(value: Value) extends Expression {
      override def rearrange: Expression = this
      override def display: String       = value.toString

      override def evaluate(unknownX: => Value): Value = value
    }
  }

  private val Root = "root"

  private def convertCommandsToExpression(
    target: Name,
    commands: Map[Name, Monkey],
    unknownName: Option[Name],
  ): Expression = {
    def translate(name: Name): Expression =
      if (unknownName.contains(name))
        UnknownX
      else
        commands(name) match {
          case Monkey.Literal(value) =>
            Literal(value)

          case Monkey.BinaryMonkey(a, operation, b) =>
            BinaryExpression(translate(a), operation, translate(b))
        }

    translate(target)
  }

  def part1(data: Parsed): Value = {
    val commands = data.toMap
    val root     = convertCommandsToExpression(Root, commands, none)
    root.rearrange match {
      case Literal(value) => value
      case other          => s"Expected Literal here but got $other".fail
    }
  }

  def part2(data: Parsed): Value = {
    val Human = "humn"

    val allCommands    = data.toMap - Human
    val rootCommand    = allCommands(Root)
    val (rootA, rootB) = rootCommand match {
      case c: Monkey.BinaryMonkey => (c.a, c.b)
      case _                      => sys.error(s"Unexpectedly $rootCommand is not a binary command")
    }

    val commands: Map[Name, Monkey] = allCommands - Root

    val leftSide  = convertCommandsToExpression(rootA, commands, Human.some)
    val rightSide = convertCommandsToExpression(rootB, commands, Human.some)

    println(s"${leftSide.display} = ${leftSide.display}")

    val rearrangedLeft  = leftSide.rearrange
    val rearrangedRight = rightSide.rearrange

    println(s"${rearrangedLeft.display} = ${rearrangedRight.display}")

    val (expression, expectedValue) = (rearrangedLeft, rearrangedRight) match {
      case (a, Literal(value)) => (a, value)
      case (Literal(value), a) => (a, value)
      case _                   =>
        s"Don't know how to handle an equation where both sides have unknowns ${rearrangedLeft.display} == ${rearrangedRight.display}".fail
    }

    @tailrec
    def solveEquation(expression: Expression, equalsValue: Value): Value =
      expression match {
        case Expression.UnknownX                       => equalsValue
        case BinaryExpression(a, Plus, Literal(b))     =>
          solveEquation(a, equalsValue - b)
        case BinaryExpression(a, Minus, Literal(b))    =>
          solveEquation(a, equalsValue + b)
        case BinaryExpression(a, Multiply, Literal(b)) =>
          solveEquation(a, equalsValue / b)
        case BinaryExpression(a, Divide, Literal(b))   =>
          solveEquation(a, equalsValue * b)
        case _                                         =>
          s"Expected the right side to be rearranged to always be a literal, but got ${expression.display}".fail
      }

    val result = solveEquation(expression, expectedValue)
    println(s"Found result $result")

    val resultLeft  = rearrangedLeft.evaluate(result)
    val resultRight = rearrangedRight.evaluate(result)
    val success     = resultLeft == resultRight
    println(s"$resultLeft ${if (success) "==" else "!="} $resultRight")
    resultLeft shouldEqual resultRight
    println()

    result
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/21-test.txt")
    val realData = readFileText("2022/21.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 152
    part1(real) shouldEqual 87457751482938L

    part2(test) shouldEqual 301
    part2(real) shouldEqual 3221245824363L
  }
}
