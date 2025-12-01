package jurisk.adventofcode.y2025

import jurisk.math.absForWrappingAround
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent01 {
  type Input = List[Command]
  type N     = Long

  sealed trait Command extends Product with Serializable {
    def diff: Int = this match {
      case Command.Left(n)  => -n
      case Command.Right(n) => n
    }
  }
  object Command {
    final case class Left(n: Int)  extends Command
    final case class Right(n: Int) extends Command

    def parse(s: String): Command =
      s match {
        case s"L$n" => Left(n.toInt)
        case s"R$n" => Right(n.toInt)
        case _      => s.failedToParse
      }
  }

  final private val MAX: Int = 100

  final case class State(current: Int, timesZero: Int) {
    def apply1(command: Command): State = {
      val result     = this.copy(current =
        absForWrappingAround(this.current + command.diff, MAX)
      )
      val isThisZero = if (result.current == 0) 1 else 0
      result.copy(timesZero = result.timesZero + isThisZero)
    }

    @tailrec
    def apply2(command: Command): State =
      command match {
        case Command.Left(n) if n <= 1  =>
          this.apply1(command)
        case Command.Right(n) if n <= 1 =>
          this.apply1(command)
        case Command.Left(n)            =>
          this.apply1(Command.Left(1)).apply2(Command.Left(n - 1))
        case Command.Right(n)           =>
          this.apply1(Command.Right(1)).apply2(Command.Right(n - 1))
      }
  }

  object State {
    def make: State = State(50, 0)
  }

  def parse(input: String): Input =
    input.parseLines(Command.parse)

  private def solve(data: Input, f: (State, Command) => State): N =
    data
      .foldLeft(State.make) {
        f(_, _)
      }
      .timesZero

  def part1(data: Input): N =
    solve(data, _ apply1 _)

  def part2(data: Input): N =
    solve(data, _ apply2 _)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/01$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
