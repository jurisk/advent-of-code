package jurisk.adventofcode.y2022

import jurisk.utils.FileInput.readLineGroups
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.SortedMap

object Advent05 {
  type Input  = (Stacks, List[Command])
  type Output = String

  type Stacks  = SortedMap[StackId, List[Crate]]
  type StackId = Int
  type Crate   = Char

  sealed trait CraneType {
    def liftCrates(crates: List[Crate]): List[Crate]
  }

  object CraneType {
    final case object CrateMover9000 extends CraneType {
      def liftCrates(crates: List[Crate]): List[Crate] = crates.reverse
    }

    final case object CrateMover9001 extends CraneType {
      def liftCrates(crates: List[Crate]): List[Crate] = crates
    }
  }

  final case class Command(amount: Int, from: StackId, to: StackId)

  object Command {
    private val Pattern = """move (\d+) from (\d+) to (\d+)""".r

    def parse(s: String): Command =
      s match {
        case Pattern(amount, fromStack, toStack) =>
          Command(amount.toInt, fromStack.toInt, toStack.toInt)

        case _ =>
          sys.error(s"Failed to match $s")
      }
  }

  private def parseStacks(stacks: List[String]): Stacks = {
    val stackIdLine = stacks.last
    val stackIds    = stackIdLine
      .split("\\s+")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toList.singleElementUnsafe)

    SortedMap.from {
      stackIds.toList.map { stackId =>
        val index    = stackIdLine.indexOf(stackId)
        val contents = stacks.reverse.tail.map(_.lift(index))
        (
          stackId.toInt - '0',
          contents.filterNot(_.contains(' ')).flatten.reverse,
        )
      }
    }
  }

  def parse(fileName: String): Input = {
    val (stacks, commands) = readLineGroups(fileName).twoElementsUnsafe
    (parseStacks(stacks), commands map Command.parse)
  }

  private def applyCommand(
    stacks: Stacks,
    command: Command,
    craneType: CraneType,
  ): Stacks = {
    val oldFrom = stacks(command.from)
    val oldTo   = stacks(command.to)

    val newFrom = oldFrom.drop(command.amount)
    val newTo   = craneType.liftCrates(oldFrom.take(command.amount)) ++ oldTo

    stacks + (command.from -> newFrom) + (command.to -> newTo)
  }

  private def solve(data: Input, craneType: CraneType): Output = {
    val (stacks, commands) = data

    val resulting = commands.foldLeft(stacks) { (acc, command) =>
      applyCommand(acc, command, craneType)
    }

    val result = resulting.values.map(_.head).mkString
    println(result)
    result
  }

  def part1(data: Input): Output = solve(data, CraneType.CrateMover9000)
  def part2(data: Input): Output = solve(data, CraneType.CrateMover9001)

  def main(args: Array[String]): Unit = {
    val test = parse("2022/05-test.txt")
    val real = parse("2022/05.txt")

    part1(test) shouldEqual "CMZ"
    part1(real) shouldEqual "MQTPGLLDN"

    part2(test) shouldEqual "MCD"
    part2(real) shouldEqual "LVZPSTTCZ"
  }
}
