package jurisk.adventofcode.y2023

import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import jurisk.algorithms.pathfinding.AStar
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Direction2D.E
import jurisk.geometry.Direction2D.S
import jurisk.geometry.Field2D
import jurisk.geometry.Rotation.Left90
import jurisk.geometry.Rotation.NoRotation
import jurisk.geometry.Rotation.Right90
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent17 {
  type Input = Field2D[Int]

  def parse(input: String): Input =
    Field2D.parseDigitField(input)

  final case class State(
    coords: Coords2D,
    direction: Option[CardinalDirection2D],
    singleDirectionCounter: Int,
  )

  private def successors(
    data: Input,
    atMostInSingleDirection: Int,
    minimumBeforeTurning: Int,
  )(state: State): List[(State, Int)] = {
    val candidateDirections = state.direction match {
      case Some(lastDirection) =>
        val turns     =
          if (state.singleDirectionCounter >= minimumBeforeTurning) {
            // Allowed to turn
            List(Left90, Right90)
          } else Nil
        val straights =
          if (state.singleDirectionCounter < atMostInSingleDirection) {
            // Allowed to go straight
            List(NoRotation)
          } else Nil

        (turns ::: straights) map lastDirection.rotate

      case None =>
        // Start
        E :: S :: Nil
    }

    candidateDirections flatMap { direction =>
      val nextCoords = state.coords + direction
      data.at(nextCoords) map { heatLoss =>
        val newState = State(
          nextCoords,
          direction.some,
          if (direction.some == state.direction) {
            state.singleDirectionCounter + 1
          } else {
            1
          },
        )
        newState -> heatLoss
      }
    }
  }

  private def solve(
    data: Input,
    atMostInSingleDirection: Int,
    minimumBeforeStoppingOrTurning: Int,
  ): Int = {
    val calculateSuccessors = successors(
      data,
      atMostInSingleDirection,
      minimumBeforeStoppingOrTurning,
    ) _

    val startState =
      State(coords = data.topLeft, none, singleDirectionCounter = 0)
    val goal       = data.bottomRight

    val result = AStar.aStar[State, Int](
      startState,
      calculateSuccessors,
      _.coords manhattanDistance goal,
      s =>
        s.coords == goal && s.singleDirectionCounter >= minimumBeforeStoppingOrTurning,
    )

    result match {
      case Some((_, result)) => result
      case None              => "Failed to solve".fail
    }
  }

  def part1(data: Input): Int =
    solve(data, 3, 0)

  def part2(data: Input): Int =
    solve(data, 10, 4)

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
