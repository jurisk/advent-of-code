package jurisk.adventofcode.y2023

import cats.implicits.catsSyntaxUnorderedFoldableOps
import jurisk.algorithms.pathfinding.Bfs
import jurisk.geometry.{Area2D, Coords2D, Direction2D, Field2D}
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent18 {
  type Input = List[Command]

  final case class Command(
    direction: CardinalDirection2D,
    meters: Int,
    color: String,
  )
  object Command {
    def parse(s: String): Command =
      s match {
        case s"$dir $meters ($color)" =>
          Command(Direction2D.parseUDLR(dir), meters.toInt, color)
        case _                        => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parseLines(Command.parse)

  sealed trait Square
  object Square {
    case object Outside extends Square
    case object Dug     extends Square
    case object Unknown extends Square
  }

  def part1(data: Input): Long = {
    var points  = Set[Coords2D](Coords2D.Zero)
    var current = Coords2D.Zero
    data foreach { command =>
      0 until command.meters foreach { _ =>
        current = current + command.direction
        points += current
      }
    }

    val bb = Area2D.boundingBoxInclusive(points.toSeq)

    val boundary = points.map(x => x - bb.topLeft)

    var dug: Set[Coords2D] = boundary

    val boundaryArea = Area2D.boundingBoxInclusive(boundary.toSeq)

    var field: Field2D[Square] = Field2D.forArea(
      Area2D(
        min = boundaryArea.min - Coords2D(1, 1),
        max = boundaryArea.max + Coords2D(1, 1),
      ),
      Square.Unknown,
    )

    boundary foreach { bc =>
      field = field.updatedAtUnsafe(bc, Square.Dug)
    }

    def sqPr(square: Square): Char = square match {
      case Square.Outside => '.'
      case Square.Dug     => '#'
      case Square.Unknown => '?'
    }

    Field2D.printField[Square](field, sqPr)

    val reachable = Bfs.bfsReachable[Coords2D](
      Coords2D.Zero,
      x =>
        field.adjacent4(x).filter { n =>
          !field.at(n).contains(Square.Dug)
        },
    )

    reachable foreach { r =>
      field = field.updatedAtUnsafe(r, Square.Outside)
    }

    Field2D.printField[Square](field, sqPr)

    println(field.count(x => x == Square.Dug || x == Square.Unknown))

    (field.width) * (field.height) - reachable.toSet.size // TODO: not sure why +1
  }

  def part2(data: Input): Long =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/18$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
