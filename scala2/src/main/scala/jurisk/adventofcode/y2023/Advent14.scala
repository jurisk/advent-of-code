package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent14.Square.{Cube, Empty, Round}
import jurisk.geometry.{Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
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
      none,
      input,
      {
        case Round => 'O'
        case Cube  => '#'
        case Empty => '.'
      },
    )

  def slideLeft(row: Vector[Square]): Vector[Square] =
    if (row.isEmpty) {
      Vector.empty
    } else {

      val potential = row.takeWhile(_ != Cube)

      if (potential.nonEmpty) {
        val emptyCount = potential.count(_ == Empty)
        val roundCount = potential.count(_ == Round)

        val a = Vector.fill(roundCount)(Round) appendedAll Vector.fill(
          emptyCount
        )(Empty)
        a appendedAll slideLeft(row.drop(potential.length))
      } else {
        val cubes = row.takeWhile(_ == Cube)
        cubes appendedAll slideLeft(row.drop(cubes.length))
      }

    }

  def slideWest(data: Input): Input = {
    val newData = data.data.map(slideLeft)
    Field2D(newData)
  }

  def slideSouth(data: Input): Input = {
    val a = data.reverseRows
    val b = slideNorth(a)
    b.reverseRows
  }

  def slideEast(data: Input): Input = {
    val a = data.reverseColumns
    val b = slideWest(a)
    b.reverseColumns
  }

  def slideNorth(data: Input): Input = {
//    debugPrint(data)

    val a = data
      .rotate(Rotation.Left90)
      .rotate(Rotation.Left90)
      .rotate(Rotation.Left90)
//    debugPrint(a)

    val b = slideWest(a)
//    debugPrint(b)

    val result = b.rotate(Rotation.Left90)
//    debugPrint(result)

    result
  }

  def cycle(data: Input): Input = {
    val a = slideNorth(data)
    val b = slideWest(a)
    val c = slideSouth(b)
    val d = slideEast(c)
    d
  }

  def value(data: Input): Int =
    data.allCoords.map { c =>
      val v = data.atOrElse(c, Square.Empty)

      v match {
        case Square.Round => data.height - c.y
        case Square.Cube  => 0
        case Square.Empty => 0
      }
    }.sum

  def part1(data: Input): Int = {
    val slided = slideNorth(data)
    value(slided)
  }

  def cycles(data: Input, count: Int): Input =
    Simulation.runNIterationsRemovingLoops(data, count) { case (acc, _) =>
      cycle(acc)
    }
//
//    (0 until count).foldLeft(data) { case (acc, _) =>
//      cycle(acc)
//    }

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
