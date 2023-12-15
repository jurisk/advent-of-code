package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent14.Square.{Cube, Empty, Round}
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.{Field2D, Rotation}
import jurisk.utils.FileInput._
import jurisk.utils.Simulation

object Advent14 {
  type Input = Field2D[Square]

  sealed trait Square extends Product with Serializable
  object Square {
    case object Round extends Square
    case object Cube  extends Square
    case object Empty extends Square

    val Mapping: BiMap[Char, Square] = BiMap(
      'O' <-> Round,
      '#' <-> Cube,
      '.' <-> Empty,
    )
  }

  def parse(input: String): Input =
    Field2D.parseFromBiMap(input, Square.Mapping)

  def debugPrint(input: Input): Unit =
    Field2D.printFieldFromBiMap[Square](input, Square.Mapping)

  // `rotation` - the rotation needed to be applied to `data` so that the sliding becomes to the West (left)
  private def slideHelper(data: Input, rotation: Rotation): Input = {
    def slideRowLeft(row: Vector[Square]): Vector[Square] = {
      def f(row: List[Square], emptiesToAdd: Int): List[Square] = {
        import List.fill

        row match {
          case Empty :: tail =>
            f(tail, emptiesToAdd + 1)
          case Round :: tail =>
            Round :: f(tail, emptiesToAdd)
          case Cube :: tail  =>
            fill(emptiesToAdd)(Empty) ::: Cube :: f(tail, 0)
          case Nil           =>
            fill(emptiesToAdd)(Empty)
        }
      }

      f(row.toList, 0).toVector
    }

    val rotated = data rotate rotation
    val slided  = rotated mapByRows slideRowLeft
    slided rotate rotation.inverse
  }

  private[y2023] def slideWest(data: Input): Input =
    slideHelper(data, Rotation.NoRotation)

  private[y2023] def slideSouth(data: Input): Input =
    slideHelper(data, Rotation.Right90)

  private[y2023] def slideEast(data: Input): Input =
    slideHelper(data, Rotation.TurnAround)

  private[y2023] def slideNorth(data: Input): Input =
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
