package jurisk.adventofcode

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent00 {
  type Parsed = List[Command]

  sealed trait Command
  object Command {
    case object Noop extends Command
    final case class Something(
      values: List[Int]
    ) extends Command

    def parse(s: String): Command =
      s match {
        case "noop"            => Noop
        case s"something $rem" => Something(rem.extractInts)
        case _                 => s.failedToParse
      }
  }

  def parse(input: String): Parsed =
    input.parseList("\n", Command.parse)

  def part1(data: Parsed): Int =
    data.length

  def part2(data: Parsed): Int =
    data.length

  def parseFile(fileName: String): Parsed =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Parsed = parseFile("2023/00.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
