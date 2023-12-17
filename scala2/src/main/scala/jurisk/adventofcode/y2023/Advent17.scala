package jurisk.adventofcode.y2023

import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.Direction2D.{CardinalDirection2D, E, S}
import jurisk.geometry.Rotation.{Left90, NoRotation, Right90}
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._

object Advent17 {
  type Input = Field2D[Int]

  def parse(input: String): Input =
    Field2D.parseDigitField(input)

  final case class State(
    coords: Coords2D,
    direction: Option[CardinalDirection2D],
    singleDirection: Int,
  )

  def successors(data: Input, state: State): List[(State, Int)] = {
    val candidateDirections = state.direction match {
      case Some(lastDirection) =>
        if (state.singleDirection == 2) {
          // must turn
          List(Left90, Right90) map { rotation =>
            lastDirection.rotate(rotation)
          }
        } else {
          List(Left90, NoRotation, Right90) map { rotation =>
            lastDirection.rotate(rotation)
          }
        }

      case None =>
        // Start
        E :: S :: Nil
    }

    candidateDirections flatMap { direction =>
      val nextCoords = state.coords + direction
      data.at(nextCoords) map { nextValue =>
        State(
          nextCoords,
          direction.some,
          if (direction.some == state.direction) {
            state.singleDirection + 1
          } else {
            0
          },
        ) -> nextValue
      }
    }
  }

  def part1(data: Input): Int = {
    val result = Dijkstra.dijkstra[State, Int](
      State(coords = data.topLeft, none, singleDirection = 0),
      x => successors(data, x),
      _.coords == data.bottomRight,
    )

    result.get._2
  }

  def part2(data: Input): Int =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/17$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
