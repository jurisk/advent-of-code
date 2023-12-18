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

  sealed trait Square
  object Square {
    case object Outside extends Square
    case object Dug     extends Square
    case object Unknown extends Square
  }

  def part1(data: List[InputLine]): Long =
    solveFloodFill(data.map(_.part1))

  def solveFloodFill(data: List[MovementInstruction]): Long = {
    val boundary: Set[Coords2D] = {
      val points = MovementInstruction.walkEveryPoint(data)

      val bb = Area2D.boundingBoxInclusive(points)
      points.map(x => x - bb.topLeft).toSet
    }

    val boundaryArea = Area2D.boundingBoxInclusive(boundary.toSeq)

    // TODO: extract expand field +1 in each direction
    var field: Field2D[Square] = Field2D.forArea(
      Area2D(
        min = boundaryArea.min - Coords2D(1, 1),
        max = boundaryArea.max + Coords2D(1, 1),
      ),
      Square.Unknown,
    )

    // TODO: extract moving from Set[Coords2D] to Field[Boolean] ?
    boundary foreach { bc =>
      field = field.updatedAtUnsafe(bc, Square.Dug)
    }

    def sqPr(square: Square): Char = square match {
      case Square.Outside => '.'
      case Square.Dug     => '#'
      case Square.Unknown => '?'
    }

    Field2D.printField[Square](field, sqPr)

    // TODO: Field2D has Field2D.floodFillField that you could also have floodFillFromOutside with just f: T => Boolean ?
    val reachable = Bfs.bfsReachable[Coords2D](
      Coords2D.Zero - Coords2D(1, 1),
      x =>
        field.adjacent4(x).filter { n =>
          !field.at(n).contains(Square.Dug)
        },
    )

    reachable foreach { r =>
      field = field.updatedAtUnsafe(r, Square.Outside)
    }

    Field2D.printField[Square](field, sqPr)

    val a = field.count(x => x == Square.Dug || x == Square.Unknown)
    val b = field.width * field.height - reachable.toSet.size
    assert(a == b)
    a
  }

  def solvePicksShoelace(data: List[MovementInstruction]): Long = {
    val path = MovementInstruction.walkPath(data)
    Coords2D.interiorPointsIncludingBoundary(path)
  }

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
