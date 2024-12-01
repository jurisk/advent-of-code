package jurisk.adventofcode

import cats.implicits._
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent00 {
  type Input = List[Command]

  sealed trait Command extends Product with Serializable
  object Command {
    case object Noop                      extends Command
    final case class Something(
      values: List[Int]
    ) extends Command
    final case class Other(value: String) extends Command

    def parse(s: String): Command =
      s match {
        case "noop"            => Noop
        case s"something $rem" => Something(rem.extractIntList)
        case s if s.nonEmpty   => Other(s)
        case _                 => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parseLines(Command.parse)

  def part1(data: Input): Int =
    0

  def part2(data: Input): Int =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/00$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
