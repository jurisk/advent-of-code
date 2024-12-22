package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.math.lcmMany
import jurisk.utils.CollectionOps.{IndexedSeqOps, SeqOps}
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

  private type NodeId   = Int
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
    private val instructions: IndexedSeq[Instruction],
    private val mappings: IndexedSeq[Mapping],
  ) {
    def instructionAtStep(step: Long): Instruction =
      instructions.atIndexWithWraparound(step)

    def findNodesByNameFilter(
      predicate: NodeName => Boolean
    ): IndexedSeq[NodeId] =
      mappings.indices.filter(index => predicate(mapping(index).name))

    def findNodeByName(name: NodeName): NodeId =
      mappings.firstIndexWhereUnsafe(_.name === name)

    def mapping(nodeId: NodeId): Mapping = mappings(nodeId)
  }

  def parse(input: List[List[String]]): Input = {
    val List(List(instructionLine), mappingLines) = input
    val instructions                              = instructionLine.map(Instruction.parse)

    val mappingTuples: IndexedSeq[(NodeName, NodeName, NodeName)] =
      mappingLines.toIndexedSeq
        .map {
          case s"$from = ($left, $right)" => (from, left, right)
          case line                       => line.failedToParse
        }

    val mappings = mappingTuples map { case (from, left, right) =>
      def idx(name: NodeName): NodeId = mappingTuples.indexWhere {
        case (from, _, _) => from == name
      }

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
      if (isTerminal(node)) counter.asLeft
      else game.mapping(node).move(game.instructionAtStep(counter)).asRight
    }

  def part1(game: Input): Long = {
    val start  = game.findNodeByName("AAA")
    val finish = game.findNodeByName("ZZZ")

    loopAt(game, start, _ == finish)
  }

  def part2(game: Input): Long = {
    val starts   = game.findNodesByNameFilter(_.last === 'A')
    val finishes = game.findNodesByNameFilter(_.last === 'Z')

    val individualResults = starts.map(loopAt(game, _, finishes.contains))
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
