package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.{Area2D, Coords2D, Field2D}
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

  def nextCounts(
    field: Field2D[Boolean],
    counts: Field2D[Long],
  ): Field2D[Long] = {
    def debugPrint(field: Field2D[Boolean], counts: Field2D[Long]) = {
      val chf = field.mapByCoordsWithValues { case (c, v) =>
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

  def part2UsingCounts(data: Input, steps: Int): Long = {
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

  def part2(data: Input, steps: Int): Long = {
    val a = part2Impulses(data, steps)
    val b = part2Old(data, steps)
    assert(a == b)
    a
  }

  def part2Impulses(data: Input, steps: Int): Long =
    ???

  // Hopeless
  def part2Dist(data: Input, steps: Int): Long = {
    val Invalid = Int.MaxValue

    val results = Dijkstra.dijkstraAll[Coords2D, Int](
      data.start,
      data.field
        .createSuccessorsFunction({ case (a, b) => !a && !b }, false)
        .map(_.map(_ -> 1)),
    )

    val distances = results.foldLeft(data.field.map(_ => Invalid)) {
      case (acc, (c, (_, n))) =>
        acc.updatedAtUnsafe(c, n)
    }

    // TODO: bad
    val all = for {
      x <- -steps to steps
      y <- -steps to steps
    } yield Coords2D(data.start.x + x, data.start.y + y)

    def distanceToStart(c: Coords2D): Long =
      distances.at(c) match {
        case Some(value) => value
        case None        =>
          // TODO: bad
          ???
      }

    all.count { c =>
      val distance = distanceToStart(c)
      steps >= distance && steps % 2 == distance % 2 && distance != Invalid
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
