package jurisk.adventofcode

import cats.implicits._
import jurisk.utils.Parsing.{PrefixRemover, StringOps}
import jurisk.utils.Utils.IterableOps
import jurisk.utils.FileInput._
import org.scalatest.matchers.should.Matchers._

object AdventTemplate {
  type Parsed = List[Command]

  sealed trait Command
  object Command {
    final case object Noop extends Command
    final case class Something(
      values: List[Int]
    ) extends Command

    def parse(s: String): Command = {
      val SomethingPrefix = PrefixRemover("something")

      s match {
        case "noop"               => Noop
        case SomethingPrefix(rem) => Something(rem.extractInts)
        case _                    =>s"Failed to parse $s".fail
      }
    }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Command.parse)

  def part1(data: Parsed): Int =
    data.length

  def part2(data: Parsed): String =
    data.counts.toString

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/00-test.txt")
    // val testData = """""";
    val realData = readFileText("2022/00.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 12345678
    part1(real) shouldEqual 12345678

    part2(test) shouldEqual "asdf"
    part2(real) shouldEqual "asdf"
  }
}
