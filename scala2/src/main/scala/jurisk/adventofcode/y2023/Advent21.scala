package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.{Area2D, Coords2D, Direction2D, Field2D}
import jurisk.math.{ArithmeticProgression, IntOps, LongOps, absForWrappingAround}
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
    println(s"slow but accurate = $a, new = $b")
    a shouldEqual b
    a
  }

  // For each type of non-center field, how many fields of such type are at each "time"
  // `inProgress` is Map[time -> counts]
  final case class InnerCounts(
    inProgress: Map[Long, Long],
    evenCorneredFinalised: Long,
    oddCorneredFinalised: Long,
  ) {
    override def toString: String = {
      val incomplete = inProgress.toList.sortBy(_._1).map { case (k, v) =>
        s"$v fields with $k protrusion"
      }.mkString(", ")

      s"$evenCorneredFinalised completed even-cornered, $oddCorneredFinalised completed odd-cornered, $incomplete"
    }
  }

  // Classify each field into categories - E, N, S, W (for "narrow cross" / "edge center") and NE, SW, SE, NW
  // (for those that flow from diagonal). They all develop similarly and we can calculate how many there are.
  //
  // Plus the one special part1 (from center / start) field (but we already know how to count it from Part 1).
  final case class FieldCounts(
    edgeCenter: InnerCounts,
    corner: InnerCounts,
  ) {
    override def toString: String = s"Edge-center: $edgeCenter\nCorner: $corner\n\n"
  }

  object FieldCounts {
    def make(time: Long, size: Long): FieldCounts = {
      val squares         = time + 1
      assert(size.parity == 1)
      val halfRoundedDown = size / 2
      val halfRoundedUp   = halfRoundedDown + 1

      val edgeCenter = {
        val completedEdgeCenter = ((squares / size) - 1) max 0
        val edgeCenterMap       = {
          val leftOverEdgeCenter =
            squares - (completedEdgeCenter * size) - halfRoundedUp

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
        }

        val (evenCorneredCompleted, oddCorneredCompleted) =
          if (completedEdgeCenter % 2 == 0) {
            (completedEdgeCenter / 2, completedEdgeCenter / 2)
          } else {
            (completedEdgeCenter / 2, (completedEdgeCenter / 2) + 1)
          }
        InnerCounts(edgeCenterMap, evenCorneredCompleted, oddCorneredCompleted)
      }

      val corner = {
        val outsideCenterField   = (squares - size - 1) max 0
        val coversInOneDirection = (outsideCenterField - size + 1) / size
        val completed            = (coversInOneDirection * (coversInOneDirection + 1)) / 2
        val leftOver             = outsideCenterField - (coversInOneDirection * size)

        val map = if (leftOver <= 0) {
          Map.empty[Long, Long]
        } else if (leftOver > size) {
          Map(
            leftOver -> (coversInOneDirection + 1L),
            (leftOver % size) -> (coversInOneDirection + 2L),
          )
        } else {
          Map(leftOver -> (coversInOneDirection + 1L))
        }

        val completed2 = ArithmeticProgression(1, 1).S_n(coversInOneDirection)
        completed2 shouldEqual completed

        val completedEven = ArithmeticProgression(1, 2).S_n(coversInOneDirection.halfRoundingUp)
        val completedOdd = ArithmeticProgression(2, 2).S_n(coversInOneDirection.halfRoundingDown)
        completedEven + completedOdd shouldEqual completed

        InnerCounts(map, completedEven, completedOdd)
      }

      FieldCounts(
        edgeCenter = edgeCenter,
        corner = corner,
      )
    }
  }

  def part2FieldClassification(data: Input, steps: Int): Long = {
    val field = data.field
    assert(field.width == field.height)
    val size  = field.width
    println(s"Field of $size x $size")

    val fieldCounts = FieldCounts.make(steps, size)
    import fieldCounts._
    println(fieldCounts)

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

    def countSquares(
      t: Long,
      distanceField: Field2D[Option[Long]],
      expectedParity: Long,
    ): Long = {
      val result = distanceField.count {
        case Some(n) => t >= n && n.parity == expectedParity
        case None    => false
      }

//      distanceField.rows foreach println
//      println(s"t = $t, expectedPartiy = $expectedParity, result  $result")

      result
    }

    val centerSquares = countSquares(steps, distanceFromCenter, steps.parity)

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

    def splitFinalised(n: Long): (Long, Long) = {
      val half = n / 2
      if (n % 2 == 0) {
        (half, half)
      } else {
        (half + 1, half)
      }
    }

    val edgeSquaresFinalised = if (steps.parity == 0) {
      (fieldCounts.edgeCenter.evenCorneredFinalised * evenSquareCount + fieldCounts.edgeCenter.oddCorneredFinalised * oddSquareCount) * 4
    } else {
      (fieldCounts.edgeCenter.evenCorneredFinalised * oddSquareCount + fieldCounts.edgeCenter.oddCorneredFinalised * evenSquareCount) * 4
    }

    val edgeSquaresInProgress = fieldCounts.edgeCenter.inProgress.map {
      case (time, count) =>
        Direction2D.CardinalDirections.map { direction =>
          val coveredAtThisTimeFromDirection: Long =
            countSquares(
              time - 1,
              distanceFromDirection(direction),
              (time + 1).parity,
            )
          coveredAtThisTimeFromDirection * count
        }.sum
    }.sum

    val cornerSquaresFinalised = if (steps.parity == 0) {
      (corner.oddCorneredFinalised * oddSquareCount + corner.evenCorneredFinalised * evenSquareCount) * 4
    } else {
      (corner.oddCorneredFinalised * evenSquareCount + corner.evenCorneredFinalised * oddSquareCount) * 4
    }
//
//    val cornerSquaresFinalised = (cornerOddFinalised * oddSquareCount + cornerEvenFinalised * evenSquareCount) * 4

    val cornerSquaresInProgress = fieldCounts.corner.inProgress.map {
      case (time, count) =>
        Direction2D.DiagonalDirections.map { direction =>
          val coveredAtThisTimeFromDirection: Long =
            countSquares(
              time - 1,
              distanceFromDirection(direction),
              (time + 1).parity,
            )
          coveredAtThisTimeFromDirection * count
        }.sum
    }.sum

    println(s"centerSquares = $centerSquares")
    println(s"edgeSquaresFinalised = $edgeSquaresFinalised")
    println(s"edgeSquaresInProgress = $edgeSquaresInProgress")
    println(s"cornerSquaresFinalised = $cornerSquaresFinalised")
    println(s"cornerSquaresInProgress = $cornerSquaresInProgress")

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
