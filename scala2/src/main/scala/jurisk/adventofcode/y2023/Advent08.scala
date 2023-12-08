package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.math.lcmMany
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent08 {
  sealed trait LeftRight
  object LRLeft  extends LeftRight
  object LRRight extends LeftRight

  type Node = String

  final case class Mapping(
    from: Node,
    left: Node,
    right: Node,
  )

  final case class Input(
    instructions: List[LeftRight],
    mapping: Map[Node, Mapping],
  )

  def parse(input: String): Input = {
    val List(a, b)   = input.split("\n\n").toList
    val instructions = a.toList.map {
      case 'L' => LRLeft
      case 'R' => LRRight
      case _   => sys.error("asdf")
    }

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
          val here = game.mapping(state)
          val qq   = next match {
            case LRLeft  => here.left
            case LRRight => here.right
          }
          qq.asRight

        }
    }

    result.toInt
  }

  def part2(game: Input): Long = {
    val startNodes = game.mapping.keySet.filter(_.last == 'A')

    val test = startNodes map { node =>
      val result = Simulation.detectLoop((node, 0)) {
        case ((state, nextIdx), counter) =>
          if (counter % 1_000_000 == 0) {
            println(s"$state $nextIdx $counter")
          }

          if (state.last == 'Z') {
            counter.asLeft
          } else {
            val next =
              game.instructions(nextIdx)

            val here = game.mapping(state)
            val nextState =
              next match {
                case LRLeft  => here.left
                case LRRight => here.right
              }

            val followingIdx = (nextIdx + 1) % game.instructions.length
            (nextState, followingIdx).asRight

          }
      }

      result match {
        case Left(value) => value
        case Right(value) => sys.error(s"$value")
      }
    }

    lcmMany(test.toList)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/08.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
