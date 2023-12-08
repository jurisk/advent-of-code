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

  private type NodeId = Int
  private type NodeName = String

  final case class Mapping(
    name: NodeName,
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
    mappings: IndexedSeq[Mapping],
  ) {
    def instructionAtStep(step: Long): Instruction = instructions(
      (step % instructions.length).toInt
    )

    def findNodesByNameFilter(predicate: NodeName => Boolean): IndexedSeq[NodeId] = mappings.indices.filter(index => predicate(mapping(index).name))

    def findNodeByName(name: NodeName): NodeId = {
      val result = mappings.indexWhere(_.name === name)
      if (result === -1) {
        sys.error(s"Did not find node $name")
      } else {
        result
      }
    }

    def mapping(nodeId: NodeId): Mapping = mappings(nodeId)
  }

  def parse(input: List[List[String]]): Input = {
    val List(List(instructionLine), mappingLines) = input
    val instructions                              = instructionLine.map(Instruction.parse)

    val mappingTuples: IndexedSeq[(NodeName, NodeName, NodeName)] = mappingLines
      .toIndexedSeq
      .map {
        case s"$from = ($left, $right)" => (from, left, right)
        case line                       => line.failedToParse
      }

    val mappings = mappingTuples map { case (from, left, right) =>
      def idx(name: NodeName): NodeId = mappingTuples.indexWhere {case (from, _, _) => from == name }

      Mapping(
        name = from,
        left = idx(left),
        right = idx(right),
      )
    }

    Input(
      instructions = instructions,
      mappings = mappings,
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

  def part1(game: Input): Long = {
    val start = game.findNodeByName("AAA")
    val finish = game.findNodeByName("ZZZ")

    loopAt(game, start, _ == finish)
  }

  def part2(game: Input): Long = {
    val startNodes = game.findNodesByNameFilter(_.last === 'A')
    val finishNodes = game.findNodesByNameFilter(_.last === 'Z')

    val individualResults = startNodes.map(loopAt(game, _, finishNodes.contains))

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
