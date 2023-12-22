package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.geometry.{Area2D, Coords2D, Direction2D, Field2D}
import jurisk.math.{IntOps, LongOps, absForWrappingAround}
import jurisk.utils.CollectionOps.{IndexedSeqOps, IterableOps}
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

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
    assert(size.parity == 1)
    val halfRoundedDown = size / 2
    val halfRoundedUp   = halfRoundedDown + 1

    val completedEdgeCenter = (((time + 1) / size) - 1) max 0
    val leftOverEdgeCenter  =
      (time + 1) - (completedEdgeCenter * size) - halfRoundedUp
    val edgeCenterMap       =
      if (leftOverEdgeCenter <= 0) {
        Map.empty[Long, Long]
      } else if (leftOverEdgeCenter > size) {
        Map(
          leftOverEdgeCenter -> 1L,
          (leftOverEdgeCenter % size) -> 1L,
        )
      } else {
        Map(leftOverEdgeCenter -> 1L)
      }

    FieldCounts(
      edgeCenter = InnerCounts(edgeCenterMap, completedEdgeCenter),
      corner = InnerCounts(Map.empty, 0), // TODO: implement corner
    )
  }

  def part2FieldClassification(data: Input, steps: Int): Long = {
    val field = data.field
    assert(field.width == field.height)
    val size  = field.width

    val fieldCounts = calculateFieldCounts(steps, size)

    val distanceFromCenter    = distancesFrom(field, data.start)
    val distanceFromDirection = Map(
      Direction2D.N  -> distancesFrom(
        field,
        field.topRowCoords.toVector.centerElementUnsafe,
      ),
      Direction2D.E  -> distancesFrom(
        field,
        field.rightColumnCoords.toVector.centerElementUnsafe,
      ),
      Direction2D.S  -> distancesFrom(
        field,
        field.bottomRowCoords.toVector.centerElementUnsafe,
      ),
      Direction2D.W  -> distancesFrom(
        field,
        field.leftColumnCoords.toVector.centerElementUnsafe,
      ),
      Direction2D.NE -> distancesFrom(field, field.topRightCornerCoords),
      Direction2D.SE -> distancesFrom(field, field.bottomRightCornerCoords),
      Direction2D.SW -> distancesFrom(field, field.bottomLeftCornerCoords),
      Direction2D.NW -> distancesFrom(field, field.topLeftCornerCoords),
    )

    def countSquares(t: Long, distanceField: Field2D[Option[Long]]): Long =
      distanceField.count {
        case Some(n) => t >= n && n.parity == t.parity
        case None    => false
      }
    val centerSquares                                                     = countSquares(steps, distanceFromCenter)

    val oddSquareCount = field.allCoords.count { c =>
      if (field.at(c).contains(false)) {
        c.manhattanDistanceToOrigin % 2 == 1
      } else {
        false
      }
    }

    val evenSquareCount = field.allCoords.count { c =>
      if (field.at(c).contains(false)) {
        c.manhattanDistanceToOrigin % 2 == 0
      } else {
        false
      }
    }

    // TODO: actually also use evenSquareCount
    val edgeSquaresFinalised = fieldCounts.edgeCenter.finalised * oddSquareCount

    val edgeSquaresInProgress = fieldCounts.edgeCenter.inProgress.map {
      case (time, count) =>
        Direction2D.CardinalDirections.map { direction =>
          val coveredAtThisTimeFromDirection: Long =
            countSquares(time, distanceFromDirection(direction))
          coveredAtThisTimeFromDirection * time * count
        }.sum
    }.sum

    // TODO: actually also use evenSquareCount
    val cornerSquaresFinalised = fieldCounts.corner.finalised * oddSquareCount

    val cornerSquaresInProgress = fieldCounts.corner.inProgress.map {
      case (time, count) =>
        Direction2D.DiagonalDirections.map { direction =>
          val coveredAtThisTimeFromDirection: Long =
            countSquares(time, distanceFromDirection(direction))
          coveredAtThisTimeFromDirection * time * count
        }.sum
    }.sum

    centerSquares + edgeSquaresFinalised + edgeSquaresInProgress + cornerSquaresFinalised + cornerSquaresInProgress
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
