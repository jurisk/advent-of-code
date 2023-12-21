package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.{Area2D, Coords2D, Field2D}
import jurisk.math.absForWrappingAround
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.immutable

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

    assert(field.allEdgeCoords.forall(c => field.at(c).contains(false)))

    Input(
      field,
      start,
    )
  }

  def part1Dist(data: Input, steps: Int): Long = {
    val results = Dijkstra.dijkstraAll[Coords2D, Int](
      data.start,
      data.field
        .createSuccessorsFunction({ case (a, b) => !a && !b }, false)
        .map(_.map(_ -> 1)),
    )

    val distances = results.foldLeft(data.field.map(_ => Int.MaxValue)) {
      case (acc, (c, (_, n))) =>
        acc.updatedAtUnsafe(c, n)
    }

    val canReach =
      distances.map(n => steps >= n && n % 2 == steps % 2 && n != Int.MaxValue)

    canReach.count(identity)
  }

  def part1(data: Input, steps: Int): Long = {
    val a = part1Dist(data, steps)
    val b = part1Simulate(data, steps)
    assert(a == b)
    a
  }

  def part1Simulate(data: Input, steps: Int): Long = {
    def debugPrint(set: Set[Coords2D]) = {
      val chf = data.field.mapByCoordsWithValues { case (c, v) =>
        if (v) {
          '█'
        } else {
          if (set.contains(c)) {
            '░'
          } else {
            ' '
          }
        }
      }

      Field2D.printCharField(chf)
    }

    val positions = Set(data.start)
    val results   = Simulation.runNIterations(positions, steps) {
      case (current, counter) =>
        println(counter)
        debugPrint(current)
        println()

        current flatMap { c =>
          data.field.adjacent4Where(c, _ == false)
        }
    }

    debugPrint(results)

    results.size
  }

  // Working but slow
  def part2Old(data: Input, steps: Int): Long = {
    def debugPrint(area: Area2D, set: Set[Coords2D]): Unit = {
      val chf = Field2D.forArea(area, ' ').mapByCoords { c =>
        val normalized = wrapCoords(data.field, c)
        if (data.field.at(normalized).contains(true)) {
          '█'
        } else {
          if (set.contains(c)) {
            '░'
          } else {
            ' '
          }
        }
      }

      Field2D.printCharField(chf)
    }

    val field                    = data.field
    val positions: Set[Coords2D] = Set(data.start)
    val results                  = Simulation.runNIterations(positions, steps) {
      case (current, counter) =>
        val dimensions = Coords2D(field.width, field.height)
        debugPrint(Area2D(dimensions * -1, dimensions * 2), current)

        val options = current.toList.flatMap { c =>
          val validNeighbours = c.adjacent4.filter { neighbour =>
            val adjusted = wrapCoords(field, neighbour)
            field.at(adjusted).contains(false)
          }

          validNeighbours
        }

        options.toSet
    }

    results.size
  }

  private def wrapCoords[T](field: Field2D[T], c: Coords2D): Coords2D = {
    val newX = absForWrappingAround(c.x, field.width)
    val newY = absForWrappingAround(c.y, field.height)
    Coords2D(newX, newY)
  }

  def part2(data: Input, steps: Int): Long = {
    val a = part2Stacked(data, steps)
    val b = part2Old(data, steps)
    a shouldEqual b
    a
  }

  def part2Interpolated(data: Input, steps: Int): Long =
    // Do a flood-fill and for each pixel (in main field), calculate number of fields that have
    // Considering checkerboard, number of fields that have it on should be calculable

    // Try to interpolate to algebraic formula that is f(pxCoords, time) -> fieldsReached
    // Then can iterate through pixels

    ???

  def part2Impulses(data: Input, steps: Int): Long =
    // We define a concept of "impulse" which is a set of incoming squares that get flipped on
    // within a field, and a set of outgoing squares that get flipped outside of square

    // These will likely be repeating in patterns.

    // These patterns determine the concept of "FieldType".

    // These "FieldType"-s will repeat with some regularity.

    // Thus we have one mapping which for each field determines "FieldType" and another which determines
    // end-state.

    // But how to avoid having to iterate through all fields?

    ???

  def part2Stacked(data: Input, steps: Int): Long = {
    // Let us build info about each pixel, at what time N it gets flipped on for each field, basically a sum of such pixels.
    // Can such N may be a linear equation from which field it is?
    // Then we can iterate through pixels (only 113x113) and solve this linear equation.

    final case class Knowledge(firstOn: Long) {
      def guess(n: Long): Boolean =
        if (n < firstOn) false else firstOn % 2 == n % 2
    }

    val field                               = data.field
    val positions: Map[Coords2D, Knowledge] = Map(data.start -> Knowledge(0L))
    val results                             = Simulation.runNIterations(positions, steps) {
      case (current, counter) =>
//        val dimensions = Coords2D(field.width, field.height)
//        debugPrint(Area2D(dimensions * -1, dimensions * 2), current)

        val options = current.toList.flatMap { case (c, knowledge) =>
          if (knowledge.guess(counter)) {
            val validNeighbours = c.adjacent4.filter { neighbour =>
              val adjusted = wrapCoords(field, neighbour)
              field.at(adjusted).contains(false)
            }

            validNeighbours map { _ -> (counter + 1L) }
          } else {
            Nil
          }
        }

        var results = current
        options.foreach { case (k, v) =>
          current.get(k) match {
            case Some(value) => assert(value.guess(counter + 1))
            case None        => results = results + (k -> Knowledge(v))
          }
        }

        results
    }

    val grouped: Map[Coords2D, List[Long]] =
      results.groupBy { case (k, v) => wrapCoords(data.field, k) }.map {
        case (k, v) =>
          k -> v.values.map(_.firstOn).toList.sorted
      }

    println(grouped)

    results.count { case (_, v) =>
      v.guess(steps)
    }
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
