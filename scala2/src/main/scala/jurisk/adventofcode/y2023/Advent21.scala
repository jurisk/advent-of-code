package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.math.absForWrappingAround
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Simulation

object Advent21 {
  final case class Input(
    field: Field2D[Boolean],
    start: Coords2D,
  )

  def parse(input: String): Input = {
    val temp  = Field2D.parseCharField(input)
    val start = temp.filterCoordsByValue(_ == 'S').singleResultUnsafe

    val field = temp.map {
      case '.' => false
      case 'S' => false
      case '#' => true
    }

    Input(
      field,
      start,
    )
  }

  def part1(data: Input, steps: Int): Int = {
    val positions = Set(data.start)
    val results   = Simulation.runNIterations(positions, steps) {
      case (current, counter) =>
        current flatMap { c =>
          data.field.adjacent4Where(c, _ == false)
        }
    }
    results.size
  }

  def part2Old(data: Input, steps: Int): Long = {
    val field                    = data.field
    val positions: Set[Coords2D] = Set(data.start)
    val results                  = Simulation.runNIterations(positions, steps) {
      case (current, counter) =>
        val options = current.toList.flatMap { c =>
          val validNeighbours = c.adjacent4.filter { neighbour =>
            val newX     = absForWrappingAround(neighbour.x, field.width)
            val newY     = absForWrappingAround(neighbour.y, field.height)
            val adjusted = Coords2D(newX, newY)
            field.at(adjusted).contains(false)
          }

          validNeighbours
        }

        options.toSet
    }

    results.size
  }

  private def wrapCoords[T](field: Field2D[T], c: Coords2D): Coords2D = {
    val newX     = absForWrappingAround(c.x, field.width)
    val newY     = absForWrappingAround(c.y, field.height)
    Coords2D(newX, newY)
  }

  private def debugPrint(field: Field2D[Boolean], counts: Field2D[Long]) = {
    val chf = field.mapByCoordsWithValues {  case (c, v) =>
      if (v) {
        '█'
      } else {
        val n = counts.at(c).get
        if (n > 10) {
          '*'
        } else {
          n.toString.head
        }
      }
    }

    Field2D.printCharField(chf)
  }

  def nextCounts(field: Field2D[Boolean], counts: Field2D[Long]): Field2D[Long] = {
    println("Before")
    debugPrint(field, counts)

    var wraparoundQueue: List[(Coords2D, Long)] = List.empty

    val within: Field2D[Long] = counts.mapByCoordsWithValues { case (c, v) =>
      if (field.at(c).contains(false)) {
        val neighbouring = c.adjacent4.flatMap { neighbour =>
          val wrapped = wrapCoords(counts, neighbour)

          if (wrapped == neighbour) {
            // neighbour is within our plate
            counts.at(neighbour)
          } else {
            // neighbour is outside of our plate
            if (field.at(wrapped).contains(false)) {
              wraparoundQueue = (wrapped -> v) :: wraparoundQueue
            }

            none
          }
        }

        neighbouring.max
      } else {
        0L
      }
    }

    val result = wraparoundQueue.foldLeft(within) { case (acc, (c, n)) =>
      acc.modifyUnsafe(c, q => q + n)
    }

    println("After")
    debugPrint(field, result)
    println()

    result
  }

  def part2(data: Input, steps: Int): Long = {
    val field  = data.field
    val counts = field
      .map(_ => 0L)
      .updatedAtUnsafe(data.start, 1L)

    val results = Simulation.runNIterations(counts, steps) {
      case (current, counter) =>
        nextCounts(field, current)
    }

    results.values.sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/21$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 64)}")
    println(s"Part 2: ${part2(realData, 26501365)}")
  }
}
