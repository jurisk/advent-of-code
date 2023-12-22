package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.geometry.{Area2D, Coords2D, Direction2D, Field2D}
import jurisk.math.{IntOps, LongOps, absForWrappingAround}
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
    assert(field.centerCoordsUnsafe == start)

    Input(
      field,
      start,
    )
  }

  def distancesFrom(
    field: Field2D[Boolean],
    from: Coords2D,
  ): Field2D[Option[Long]] = {
    val results = Dijkstra.dijkstraAll[Coords2D, Int](
      from,
      field
        .createSuccessorsFunction({ case (a, b) => !a && !b }, false)
        .map(_.map(_ -> 1)),
    )

    val empty: Field2D[Option[Long]] = field.map(_ => none)
    results.foldLeft(empty) { case (acc, (c, (_, n))) =>
      acc.updatedAtUnsafe(c, n.toLong.some)
    }
  }

  def part1Dist(data: Input, steps: Int): Long = {
    val distances = distancesFrom(data.field, data.start)
    distances.count {
      case Some(n) => steps >= n && n.parity == steps.parity
      case None    => false
    }
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
//        println(counter)
//        debugPrint(current)
//        println()
//
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
//        val dimensions = Coords2D(field.width, field.height)
//        debugPrint(Area2D(dimensions * -1, dimensions * 2), current)

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
    val a = part2Old(data, steps)
    val b = part2FieldClassification(data, steps)
    println(s"$a, $b")
    a shouldEqual b
    a
  }

  // `inProgress` is Map[time -> counts]
  final case class InnerCounts(
    inProgress: Map[Long, Long],
    finalised: Long,
  )

  // TODO:  Classify each field into categories - E, N, S, W (for narrow cross) and NE, SW, SE, NW (for those that flow
  //        from diagonal). They all develop similarly and we can calculate how many there are.
  //        Plus the one special part1 (from center / start) field.
  final case class FieldCounts(
    // For each type of non-center field, how many fields of such type are at each "time"
    edgeCenter: InnerCounts,
    corner: InnerCounts,
  )

  def calculateFieldCounts(time: Long, size: Long): FieldCounts = {
    val p = time / size
    val q = time % size

    // TODO: implement
    FieldCounts(
      edgeCenter = InnerCounts(Map.empty, 123),
      corner = InnerCounts(Map.empty, 123),
    )
  }

  def part2FieldClassification(data: Input, steps: Int): Long = {
    val field = data.field
    assert(field.width == field.height)
    val size  = field.width

    val fieldCounts = calculateFieldCounts(steps, size)

    // TODO: Manhattan Distance diffs from each corner / edge center, as well as field center
    // TODO: Mapping from (Option[Direction2D], time: Long) to Long (squares covered)

    val distanceFromCenter = distancesFrom(data.field, data.start)

    val centerSquares = distanceFromCenter.count {
      case Some(n) => steps >= n && n.parity == steps.parity
      case None    => false
    }

    val edgeSquaresFinalised = fieldCounts.edgeCenter.finalised * 123 // TODO

    val edgeSquaresInProgress = fieldCounts.edgeCenter.inProgress.map {
      case (time, count) =>
        Direction2D.CardinalDirections.map { direction =>
          println(direction)
          val coveredAtThisTimeFromDirection: Long = 123 // TODO
          coveredAtThisTimeFromDirection * time * count
        }.sum
    }.sum

    val cornerSquaresFinalised = fieldCounts.corner.finalised * 123 // TODO

    val cornerSquaresInProgress = fieldCounts.corner.inProgress.map {
      case (time, count) =>
        Direction2D.DiagonalDirections.map { direction =>
          println(direction)
          val coveredAtThisTimeFromDirection: Long = 123 // TODO
          coveredAtThisTimeFromDirection * time * count
        }.sum
    }.sum

    centerSquares + edgeSquaresFinalised + edgeSquaresInProgress + cornerSquaresFinalised + cornerSquaresInProgress
  }

  def part2DistRec(data: Input, steps: Int): Long = {
    val field = data.field

    // Narrow cross should be empty
    assert(field.column(field.width / 2).forall(b => !b))
    assert(field.row(field.height / 2).forall(b => !b))

    // Edges should be empty
    assert(field.allEdgeCoords.forall(c => field.at(c).contains(false)))

    val start     = data.start
    val distances = distancesFrom(data.field, data.start)

    val manhattanDiffs = distances.mapByCoordsWithValues { case (c, v) =>
      v map { v =>
        v - c.manhattanDistance(start)
      }
    }

    assert(
      manhattanDiffs.allEdgeCoords.forall(c =>
        manhattanDiffs.at(c).flatten == 0.some
      )
    )

    val printableManhattanDiffs = manhattanDiffs.map {
      case None    => "R"
      case Some(0) => ""
      case Some(v) => v.toString
    }

    Field2D.printStringField(printableManhattanDiffs, 3)

    def distance(c: Coords2D): Long = {
      val brutal = distanceBrutal(c)
      val smart  = distanceSmart(c)
      brutal shouldEqual smart
      brutal
    }

    def distanceBrutal(c: Coords2D): Long =
      Dijkstra
        .dijkstraWithIdenticalCosts[Coords2D, Long](
          c,
          c =>
            c.adjacent4.filter { n: Coords2D =>
              val w = wrapCoords(field, n)
              field.at(w).contains(false)
            },
          _ == start,
        )
        .get
        ._2

    def distanceSmart(c: Coords2D): Long = {
      // TODO:
      // Is it in original field? Then we have the distance?
      // Is it further out? Find the offset, take naive MD, adjust somehow?
      // Or do we have to somehow check distances from corners / edge midpoints?
      // It could be we have to treat the "on the narrow cross" fields separately (calc from edge midpoint) and the
      // others separately (more naively)

      val md      = c.manhattanDistance(start)
      val wrapped = wrapCoords(field, c)

      // TODO:  Not just the diff from center, diff from either the center of the edge (for narrow cross) or from the
      //        corner closest to the start
      val diff = manhattanDiffs.at(wrapped).flatten.get
      md + diff
//
//      if (distances.isValidCoordinate(c)) {
//        distances.at(c).flatten.get
//      } else {
//        val fieldOffset = Coords2D(
//          c.x / field.width,
//          c.y / field.height,
//        )
//
//        println(s"field offset = $fieldOffset for coords $c")
//        ???
//      }
    }

    // TODO: Can we just iterate the width x height, and for each pixel, based on `steps`, decide in how many
    //       fields it is "on"? Binary search as a last resort?

    def isOn(c: Coords2D): Boolean = {
      val wrapped = wrapCoords(field, c)
      if (field.at(wrapped).contains(false)) {
        val d = distance(c)
        d <= steps && d.parity == steps.parity
      } else {
        false
      }
    }

    // TODO: we iterate too much, we should just iterate the diamond shape, not a square
    val result =
      (-steps to +steps)
        .flatMap { x =>
          (-steps to +steps)
            .map { y =>
              Coords2D(x, y) + data.start
            }
        }
        .count { c =>
          isOn(c)
        }

    result
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
        if (n < firstOn) false else firstOn.parity == n.parity
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

//    println(grouped)

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
