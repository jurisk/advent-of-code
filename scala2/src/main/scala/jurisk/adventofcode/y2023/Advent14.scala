package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent14.Square.{Cube, Empty, Round}
import jurisk.geometry.{Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Simulation

object Advent14 {
  type Input = Field2D[Square]

  sealed trait Square
  object Square {
    case object Round extends Square
    case object Cube  extends Square
    case object Empty extends Square

    def parse(ch: Char): Square =
      Map('O' -> Round, '#' -> Cube, '.' -> Empty).apply(ch)
  }

  def parse(input: String): Input =
    Field2D.parse(input, Square.parse)

  def debugPrint(input: Input): Unit =
    Field2D.printField[Square](
      input,
      {
        case Round => 'O'
        case Cube  => '#'
        case Empty => '.'
      },
    )

  private def slideRowLeft(row: Vector[Square]): Vector[Square] = {
    import Vector.fill

    val potential = row.takeWhile(_ != Cube)

    if (potential.nonEmpty) {
      val emptyCount = potential.count(_ == Empty)
      val roundCount = potential.count(_ == Round)

      fill(roundCount)(Round) ++ fill(emptyCount)(Empty) ++ slideRowLeft(
        row.drop(potential.length)
      )
    } else {
      val cubes = row.takeWhile(_ == Cube)
      if (cubes.isEmpty) {
        Vector.empty
      } else {
        cubes ++ slideRowLeft(row.drop(cubes.length))
      }
    }
  }

  private def slideHelper(data: Input, rotation: Rotation): Input = {
    val rotated = data.rotate(rotation)
    val slided  = Field2D(rotated.data.map(slideRowLeft))
    slided.rotate(rotation.inverse)
  }

  def slideWest(data: Input): Input =
    slideHelper(data, Rotation.NoRotation)

  def slideSouth(data: Input): Input =
    slideHelper(data, Rotation.Right90)

  def slideEast(data: Input): Input =
    slideHelper(data, Rotation.TurnAround)

  def slideNorth(data: Input): Input =
    slideHelper(data, Rotation.Left90)

  def cycle(data: Input): Input =
    (slideNorth _ andThen slideWest andThen slideSouth andThen slideEast)(data)

  def value(data: Input): Int =
    data.valuesAndCoords.map { case (c, v) =>
      v match {
        case Round        => data.height - c.y
        case Cube | Empty => 0
      }
    }.sum

  def part1(data: Input): Int =
    (slideNorth _ andThen value)(data)

  def cycles(data: Input, count: Int): Input =
    Simulation.runNIterationsRemovingLoops(data, count) { case (acc, _) =>
      cycle(acc)
    }

  def part2(data: Input): Int =
    value(cycles(data, 1000000000))

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/14.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
