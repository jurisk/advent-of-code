package jurisk.adventofcode.y2024

import cats.implicits._
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent06 {
  type Input = (State, Field2D[Boolean])
  final case class State(location: Coords2D, direction: CardinalDirection2D, visited: Set[Coords2D])
  final case class State2(location: Coords2D, direction: CardinalDirection2D)

  def parse(input: String): Input = {
    val charField = Field2D.parseCharField(input)
    val result = charField.map {
      case '.' => false
      case '#'  => true
      case '^' => false
      case ch => ch.toString.fail
    }
    val location = charField.allCoords.find(charField(_).contains('^')).get
    val state = State(location, Direction2D.N, Set(location))
    (state, result)
  }

  def part1(data: Input): Int = {
    val (state, field) = data
    var s = state
    while (field.at(s.location).isDefined) {
      val nextLocation = s.location + s.direction
      field.at(nextLocation) match {
        case Some(false) =>
          s = s.copy(location = nextLocation, visited = s.visited + nextLocation)
        case Some(true) =>
          s = s.copy(direction = s.direction.rotate(Rotation.Right90))
        case None =>
          return s.visited.size
      }
    }
    s.visited.size
  }

  private def wouldLoop(c: Coords2D, state: State, field: Field2D[Boolean]): Boolean = {
    println(s"$c")

    val hypothetical = field.updatedAtUnsafe(c, true)
    var seenStates: Set[State2] = Set()
    var s: State2 = State2(state.location, state.direction)
    while (true) {
      val nextLocation = s.location + s.direction
      if (seenStates.contains(s)) {
        return true
      } else {
          seenStates += s
      }

      hypothetical.at(nextLocation) match {
        case Some(false) =>
          s = s.copy(location = nextLocation)
        case Some(true) =>
          s = s.copy(direction = s.direction.rotate(Rotation.Right90))
        case None =>
          return false
      }
    }
    false
  }

  def part2(data: Input): Int = {
    val (state, field) = data

    field.allCoords.filterNot(_ == state.location).count(x => wouldLoop(x, state, field))
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/06$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
