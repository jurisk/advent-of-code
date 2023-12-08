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

  type Node = String

  final case class Mapping(
    from: Node,
    left: Node,
    right: Node,
  ) {
    def move(instruction: Instruction): Node =
      instruction match {
        case Instruction.Left  => left
        case Instruction.Right => right
      }
  }

  final case class Input(
    instructions: IndexedSeq[Instruction],
    mapping: Map[Node, Mapping],
  )

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
    start: Node,
    isTerminal: Node => Boolean,
  ): Long = {
    val result = Simulation.detectLoop((start, 0)) {
      case ((state, nextIdx), counter) =>
        if (isTerminal(state)) {
          counter.asLeft
        } else {
          val next         = game.instructions(nextIdx)
          val nextState    = game.mapping(state).move(next)
          val followingIdx = (nextIdx + 1) % game.instructions.length
          (nextState, followingIdx).asRight
        }
    }

    result match {
      case Left(value)  => value
      case Right(value) => sys.error(s"$value")
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
