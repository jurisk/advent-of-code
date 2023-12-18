package jurisk.adventofcode.y2023

import cats.implicits.catsSyntaxUnorderedFoldableOps
import jurisk.algorithms.pathfinding.Bfs
import jurisk.geometry._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent18 {
  final case class InputLine(
    part1: MovementInstruction,
    part2: MovementInstruction,
  )

  private object InputLine {
    def parse(s: String): InputLine =
      s match {
        case s"$dir $meters ($color)" =>
          val part1Direction = Direction2D.parseUDLR(dir)
          val part1Meters    = meters.toInt

          assert(color.head == '#')
          assert(color.length == 7)
          val part2Direction = color(6) match {
            case '0' => Direction2D.E
            case '1' => Direction2D.S
            case '2' => Direction2D.W
            case '3' => Direction2D.N
            case s   => s"Unrecognized direction $s".fail
          }
          val part2Meters    = Integer.parseInt(color.take(6).tail, 16)

          InputLine(
            part1 = MovementInstruction(part1Direction, part1Meters),
            part2 = MovementInstruction(part2Direction, part2Meters),
          )
        case _                        => s.failedToParse
      }
  }

  def parse(input: String): List[InputLine] =
    input.parseLines(InputLine.parse)

  sealed trait Square extends Product with Serializable
  object Square {
    case object Outside extends Square
    case object Dug     extends Square
    case object Unknown extends Square
  }

  private def printField(field: Field2D[Square]): Unit = {
    println(s"Top left: ${field.topLeft}")

    Field2D.printField[Square](
      field,
      {
        case Square.Outside => '.'
        case Square.Dug     => '#'
        case Square.Unknown => '?'
      },
    )
  }

  def solveFloodFill(data: List[MovementInstruction]): Long = {
    var field = Field2D
      .fromPoints(
        MovementInstruction.walkEveryPoint(data),
        {
          case true  => Square.Dug
          case false => Square.Unknown
        },
      )
    printField(field)

    field = field.expandOneSquareInAllDirections(Square.Unknown)

    printField(field)

    // TODO: Field2D has Field2D.floodFillCoordinates that you could also have floodFillFromOutside with just f: T => Boolean ?

    val reachable = Field2D.floodFillCoordinates[Square](
      field,
      field.topLeft,
      (_, to) => to == Square.Unknown,
    )

    reachable foreach { r =>
      field = field.updatedAtUnsafe(r, Square.Outside)
    }

    printField(field)

    val a = field.count(x => x == Square.Dug || x == Square.Unknown)
    val b = field.width * field.height - reachable.toSet.size
    assert(a == b)
    a
  }

  def solvePicksShoelace(data: List[MovementInstruction]): Long = {
    val path = MovementInstruction.walkPath(data)
    Coords2D.interiorPointsIncludingBoundary(path)
  }

  def part1(data: List[InputLine]): Long =
    solveFloodFill(data.map(_.part1))

  def part2(data: List[InputLine]): Long =
    solvePicksShoelace(data.map(_.part2))

  def parseFile(fileName: String): List[InputLine] =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/18$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: List[InputLine] = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
