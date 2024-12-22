package jurisk.adventofcode.y2024

import cats.effect.IO
import cats.effect.IOApp
import cats.implicits.catsSyntaxParallelTraverse1
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent07 extends IOApp.Simple {
  type Input = List[Equation]

  private def concatLongs(a: Long, b: Long): Long = {
    val bLength = math.log10(b.toDouble).toInt + 1
    a * math.pow(10, bLength).toLong + b
  }

  private[y2024] def f(
    allowPipe: Boolean,
    result: Long,
    numbers: List[Long],
  ): Boolean =
    numbers match {
      case Nil         => false
      case a :: Nil    => result == a
      case a :: b :: t =>
        val options = List(
          a * b,
          a + b,
        ) ::: (if (allowPipe) List(concatLongs(a, b)) else Nil)

        options.exists { r =>
          f(allowPipe, result, r :: t)
        }
    }

  final case class Equation(
    result: Long,
    numbers: List[Long],
  ) {
    def validate(allowPipe: Boolean): Boolean =
      f(allowPipe, result, numbers)
  }

  object Equation {
    def parse(s: String): Equation = {
      val (a, b) = s.parsePairUnsafe(": ", _.toLong, _.parseList(" ", _.toLong))
      Equation(a, b)
    }
  }

  def parse(input: String): Input =
    input.parseLines(Equation.parse)

  def solve(data: Input, allowPipe: Boolean): IO[Long] =
    data
      .parTraverse { equation =>
        for {
          valid <- IO(equation.validate(allowPipe))
        } yield if (valid) equation.result else 0
      }
      .map(_.sum)

  def part1(data: Input): IO[Long] =
    solve(data, allowPipe = false)

  def part2(data: Input): IO[Long] =
    solve(data, allowPipe = true)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/07$suffix.txt"

  override def run: IO[Unit] =
    for {
      realData <- IO(parseFile(fileName("")))
      result1  <- part1(realData)
      _        <- IO(println(s"Part 1: $result1"))
      result2  <- part2(realData)
      _        <- IO(println(s"Part 2: $result2"))
    } yield ()
}
