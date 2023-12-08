package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.math.lcmMany
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent08 {
  sealed trait Instruction
  object Instruction {
    case object Left  extends Instruction
    case object Right extends Instruction

    def parse(ch: Char): Instruction =
      ch match {
        case 'L' => Instruction.Left
        case 'R' => Instruction.Right
        case _   => ch.toString.failedToParse
      }
  }

  type NodeId = String

  final case class Mapping(
    from: NodeId,
    left: NodeId,
    right: NodeId,
  ) {
    def move(instruction: Instruction): NodeId =
      instruction match {
        case Instruction.Left  => left
        case Instruction.Right => right
      }
  }

  final case class Input(
    instructions: IndexedSeq[Instruction],
    mapping: Map[NodeId, Mapping],
  ) {
    def instructionAtStep(step: Long): Instruction = instructions(
      (step % instructions.length).toInt
    )
  }

  def parse(input: List[List[String]]): Input = {
    val List(List(instructionLine), mappingLines) = input
    val instructions                              = instructionLine.map(Instruction.parse)

    val mappings = mappingLines
      .map {
        case s"$from = ($left, $right)" => Mapping(from, left, right)
        case line                       => line.failedToParse
      }
      .map { x =>
        x.from -> x
      }
      .toMap

    Input(
      instructions = instructions,
      mapping = mappings,
    )
  }

  private def loopAt(
    game: Input,
    start: NodeId,
    isTerminal: NodeId => Boolean,
  ): Long =
    Simulation.runWithIterationCount(start) { case (node, counter) =>
      if (isTerminal(node)) {
        counter.asLeft
      } else {
        val next      = game.instructionAtStep(counter)
        val nextState = game.mapping(node).move(next)
        nextState.asRight
      }
    }

  def part1(game: Input): Long =
    loopAt(game, "AAA", _ == "ZZZ")

  def part2(game: Input): Long = {
    val startNodes = game.mapping.keys.filter(_.last == 'A')

    val individualResults = startNodes map { node =>
      loopAt(game, node, _.last == 'Z')
    }

    lcmMany(individualResults)
  }

  def parseFile(fileName: String): Input =
    parse(readLineGroups(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/08.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
