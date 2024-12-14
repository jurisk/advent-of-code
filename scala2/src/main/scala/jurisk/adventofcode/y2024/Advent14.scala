package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxEitherId
import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import cats.implicits.toFunctorOps
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.NE
import jurisk.geometry.Direction2D.NW
import jurisk.geometry.Direction2D.SE
import jurisk.geometry.Direction2D.SW
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

import scala.collection.immutable.BitSet

object Advent14 {
  type Input = List[(Coords2D, Coords2D)]
  type N     = Long

  private def wrapTo(what: Int, toWhat: Int): Int =
    if (what >= 0) what % toWhat else what + toWhat

  def parse(input: String): Input = {
    val Number                                  = """([-+]?\d+)"""
    val Pattern                                 = s"p=$Number,$Number v=$Number,$Number".r
    def parseL(s: String): (Coords2D, Coords2D) =
      s match {
        case Pattern(px, py, vx, vy) =>
          (Coords2D(px.toInt, py.toInt), Coords2D(vx.toInt, vy.toInt))
        case _                       => s.fail
      }

    input.parseLines(parseL)
  }

  private def printField(value: Field2D[BitSet]): String =
    Field2D.toDebugRepresentation(value.map { v =>
      if (v.isEmpty) {
        '.'
      } else if (v.size <= 10) {
        v.size.toString.head
      } else {
        '#'
      }
    })

  private def calculateSegments(
    result: Field2D[BitSet]
  ): Map[Direction2D, Int] = {
    var results = Map.empty[Direction2D, Int]

    val left   = 0 until result.width / 2
    val right  = result.width / 2 + 1 until result.width
    val top    = 0 until result.height / 2
    val bottom = result.height / 2 + 1 until result.height

    result.allCoords foreach { c =>
      val direction = (
        left.contains(c.x),
        right.contains(c.x),
        top.contains(c.y),
        bottom.contains(c.y),
      ) match {
        case (true, false, true, false) => NW.some
        case (false, true, true, false) => NE.some
        case (true, false, false, true) => SW.some
        case (false, true, false, true) => SE.some
        case _                          => none
      }

      val add = result.atOrElse(c, BitSet.empty).size

      direction foreach { direction =>
        results = results.updatedWith(direction) {
          case None        => add.some
          case Some(value) => (value + add).some
        }
      }
    }

    results
  }

  def solve(
    robots: Input,
    wide: Int,
    tall: Int,
    until: (Long, Field2D[BitSet]) => Boolean,
  ): (Long, Field2D[BitSet]) = {
    val (initialPositions, velocities) = robots.unzip

    var field: Field2D[BitSet] = Field2D.ofSize(wide, tall, BitSet.empty)
    initialPositions.zipWithIndex.foreach { case (pos, id) =>
      field = field.modifyIgnoringInvalidCoords(pos, set => set + id)
    }

    val (finalCounter, result) = Simulation.runWithIterationCount(field) {
      case (state, counter) =>
        println(s"counter: $counter")

        var newState = Field2D.ofSize(wide, tall, BitSet.empty)
        state.allCoordsAndValues.foreach { case (c, set) =>
          set foreach { robotId =>
            val newPosition = c + velocities(robotId)
            val wrapped     = newPosition.copy(
              x = wrapTo(newPosition.x, wide),
              y = wrapTo(newPosition.y, tall),
            )
            newState = newState.modifyIgnoringInvalidCoords(
              wrapped,
              set => set + robotId,
            )
          }
        }

        if (until(counter, newState)) {
          (counter, newState).asLeft
        } else {
          newState.asRight
        }
    }

    (finalCounter, result)
  }

  def part1(robots: Input, wide: Int, tall: Int): N = {
    val Steps       = 100
    val (_, result) =
      solve(robots, wide, tall, (counter, _) => counter == Steps - 1)
    calculateSegments(result).values.product
  }

  def part2(robots: Input, wide: Int, tall: Int): N = {
    def isChristmasTree(field: Field2D[BitSet]): Boolean = {
      val segments    = calculateSegments(field)
      val symmetry    = segments(NW) + segments(SW) - segments(SE) - segments(NE)
      val bottomHeavy =
        segments(SW) + segments(SE) - segments(NW) - segments(NE)

      // Found empirically
      (symmetry == 0) && (bottomHeavy > 200)
    }

    val (counter, result) =
      solve(robots, wide, tall, (_, state) => isChristmasTree(state))
    println(printField(result))
    counter + 1
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/14$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 101, 103)}")
    println(s"Part 2: ${part2(realData, 101, 103)}")
  }
}
