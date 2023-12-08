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
    instructions: List[Instruction],
    mapping: Map[Node, Mapping],
  )

  def parse(input: String): Input = {
    val List(a, b)   = input.split("\n\n").toList
    val instructions = a.map(Instruction.parse).toList

    val lines: List[String] = b.split("\n").toList
    val mappings            = lines
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

  def part1(game: Input): Long = {
    val result = Simulation.runWithIterationCount("AAA") {
      case (state, counter) =>
        if (state == "ZZZ") {
          counter.asLeft
        } else {
          val next =
            game.instructions((counter % game.instructions.length).toInt)
          game.mapping(state).move(next).asRight
        }
    }

    result.toInt
  }

  def part2(game: Input): Long = {
    val startNodes = game.mapping.keys.filter(_.last == 'A').toList

    val loops = startNodes map { node =>
      val result = Simulation.detectLoop((node, 0)) {
        case ((state, nextIdx), counter) =>
          if (state.last == 'Z') {
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

    lcmMany(loops)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/08.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
