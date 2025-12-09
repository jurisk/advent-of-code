package jurisk.adventofcode.y2025

import cats.implicits._
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent07 {
  type C = Int

  final case class Diagram(
    startX: C,
    splitters: Field2D[Boolean],
  )

  final case class State1(
    diagram: Diagram,
    currentY: C,
    rayXes: Set[C],
    splitsOccurred: Int,
  ) {
    def next: State1 = {
      val (newRayXes, splitsThisRow) = rayXes.foldLeft((Set.empty[C], 0)) {
        case ((accXes, splits), rayX) =>
          if (diagram.splitters.atOrElse(Coords2D.of(rayX, currentY), false)) {
            (accXes + (rayX - 1) + (rayX + 1), splits + 1)
          } else {
            (accXes + rayX, splits)
          }
      }

      State1(
        diagram,
        currentY + 1,
        newRayXes,
        splitsOccurred + splitsThisRow,
      )
    }
  }

  object State1 {
    def initial(diagram: Diagram): State1 =
      State1(
        diagram,
        currentY = 0,
        rayXes = Set(diagram.startX),
        splitsOccurred = 0,
      )
  }

  final case class State2(
    diagram: Diagram,
    currentY: C,
    quantumRays: Map[C, Long],
  ) {
    def next: State2 = {
      val newRays = quantumRays.foldLeft(Map.empty[C, Long]) {
        case (accRays, (rayX, count)) =>
          if (diagram.splitters.atOrElse(Coords2D.of(rayX, currentY), false)) {
            accRays
              .updatedWith(rayX - 1) {
                case Some(existing) => Some(existing + count)
                case None           => Some(count)
              }
              .updatedWith(rayX + 1) {
                case Some(existing) => Some(existing + count)
                case None           => Some(count)
              }
          } else {
            accRays.updatedWith(rayX) {
              case Some(existing) => Some(existing + count)
              case None           => Some(count)
            }
          }
      }

      State2(
        diagram,
        currentY + 1,
        newRays,
      )
    }
  }

  object State2 {
    def initial(diagram: Diagram): State2 =
      State2(
        diagram,
        currentY = 0,
        quantumRays = Map(diagram.startX -> 1),
      )
  }

  def parse(input: String): Diagram = {
    val charField = Field2D.parseCharField(input)
    val startX    = charField.row(0).indexWhere(_ == 'S') match {
      case -1  => "No starting position 'S' found in the first row".fail
      case idx => idx
    }
    val splitters = charField.map {
      case '^'       => true
      case 'S' | '.' => false
      case ch        => s"Unexpected character '$ch' in diagram".fail
    }
    Diagram(
      startX,
      splitters,
    )
  }

  def part1(data: Diagram): Int = {
    val initial = State1.initial(data)
    val result  = (0 until data.splitters.width).foldLeft(initial) {
      case (state, _) =>
        state.next
    }
    result.splitsOccurred
  }

  def part2(data: Diagram): Long = {
    val initial    = State2.initial(data)
    val finalState = (0 until data.splitters.height).foldLeft(initial) {
      case (state, _) =>
        state.next
    }
    finalState.quantumRays.values.sum
  }

  def parseFile(fileName: String): Diagram =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/07$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Diagram = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
