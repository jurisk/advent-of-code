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
    case object CrateMover9000 extends CraneType {
      def liftCrates(crates: List[Crate]): List[Crate] = crates.reverse
    }

    case object CrateMover9001 extends CraneType {
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

  private def parseStacks(input: List[String]): Stacks =
    input.reverse match {
      case stackIdLine :: stackContents =>
        val stackIdChars = stackIdLine
          .split("\\s+")
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(_.toList.singleElementUnsafe)

        SortedMap.from {
          stackIdChars map { stackIdChar =>
            val index    = stackIdLine.indexOf(stackIdChar)
            require(index != -1)
            val contents = stackContents.map(_.lift(index))
            (
              stackIdChar - '0',
              contents.filterNot(_.contains(' ')).flatten.reverse,
            )
          }
        }

      case _ =>
        sys.error(s"Invalid input $input")
    }

  def readFileAndParse(fileName: String): Input = {
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

    stacks ++ SortedMap(command.from -> newFrom, command.to -> newTo)
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
    val test = readFileAndParse("2022/05-test.txt")
    val real = readFileAndParse("2022/05.txt")

    part1(test) shouldEqual "CMZ"
    part1(real) shouldEqual "MQTPGLLDN"

    part2(test) shouldEqual "MCD"
    part2(real) shouldEqual "LVZPSTTCZ"
  }
}
