package jurisk.adventofcode.y2024

import cats.implicits._
import jurisk.adventofcode.y2024.Advent06.Block.Empty
import jurisk.adventofcode.y2024.Advent06.Block.Wall
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D
import jurisk.geometry.Rotation
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent06 {
  sealed trait Block extends Product with Serializable
  object Block {
    case object Empty extends Block
    case object Wall  extends Block
  }

  type Input = (Coords2D, Field2D[Block])

  final private case class GuardWithVisitedLog(
    guard: Guard,
    visited: Set[Coords2D],
  )

  final private case class Guard(
    location: Coords2D,
    direction: CardinalDirection2D,
  ) {
    def next(field: Field2D[Block]): Option[Guard] = {
      val nextLocation = location + direction

      field.at(nextLocation) map {
        case Empty =>
          copy(location = nextLocation)
        case Wall  =>
          copy(direction = direction.rotate(Rotation.Right90))
      }
    }
  }

  def parse(input: String): Input = {
    val charField = Field2D.parseCharField(input)
    val field     = charField.map {
      case '.' | '^' => Empty
      case '#'       => Wall
      case ch        => s"Unexpected character: $ch".fail
    }

    val location = charField
      .findCoordsByValue('^')
      .getOrElse("No starting location found".fail)

    (location, field)
  }

  private def guardsPath(data: Input): Set[Coords2D] = {
    // TODO: Try a BitSet instead of a Set?

    val (location, field) = data
    val state             =
      GuardWithVisitedLog(Guard(location, Direction2D.N), Set(location))

    Simulation.run(state) { s =>
      s.guard.next(field) match {
        case Some(next) =>
          s.copy(
            guard = next,
            visited = s.visited + next.location,
          ).asRight
        case None       =>
          s.visited.asLeft
      }
    }
  }

  def part1(data: Input): Int =
    guardsPath(data).size

  private def wouldLoop(
    location: Coords2D,
    field: Field2D[Block],
  ): Boolean =
    Simulation
      .detectLoop(Guard(location, Direction2D.N)) { case (s, _) =>
        s.next(field).toRight(())
      }
      .isRight

  def part2(data: Input): Int = {
    val (location, field) = data

    guardsPath(data)
      .filterNot(_ == location)
      .count(c => wouldLoop(location, field.updatedAtUnsafe(c, Wall)))
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/06$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
