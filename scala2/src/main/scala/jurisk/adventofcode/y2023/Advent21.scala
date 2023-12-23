package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Field2D
import jurisk.math.ArithmeticProgression
import jurisk.math.IntOps
import jurisk.math.LongOps
import jurisk.math.absForWrappingAround
import jurisk.utils.CollectionOps.IndexedSeqOps
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable

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

  private def distancesFrom(
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

  private def part1Dist(data: Input, steps: Int): Long = {
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

  private def part1Simulate(data: Input, steps: Int): Long = {
    val positions = Set(data.start)
    val results   = Simulation.runNIterations(positions, steps) {
      case (current, _) =>
        current flatMap { c =>
          data.field.adjacent4Where(c, _ == false)
        }
    }

    results.size
  }

  // Working but slow
  def part2Simulation(data: Input, steps: Int): Long = {
    def debugPrint(area: Area2D, map: mutable.HashMap[Coords2D, Long]): Long = {
      var results = 0

      val stringField = Field2D.forArea(area, ' ').mapByCoords { c =>
        val normalized = wrapCoords(data.field, c)
        if (data.field.at(normalized).contains(true)) {
          "███"
        } else {
          map.get(c) match {
            case Some(value) =>
              if (value.parity == steps.parity) {
                results += 1
              }
              value.toString
            case None        => "···"
          }
        }
      }

      Field2D.printStringField(stringField, 3)

      println(s"Results = $results")
      results
    }

    val field                                      = data.field
    val firstSeen: mutable.HashMap[Coords2D, Long] =
      mutable.HashMap(data.start -> 0)

    def updateMap(set: Set[Coords2D], counter: Long): Unit =
      set foreach { c =>
        firstSeen.updateWith(c) {
          case Some(v) => v.some
          case None    => counter.some
        }
      }

    val results = Simulation.runNIterations(Set(data.start), steps) {
      case (frontier, counter) =>
        val options = frontier.toList.flatMap { c =>
          val validNeighbours = c.adjacent4.filter { neighbour =>
            val adjusted = wrapCoords(field, neighbour)
            field.at(adjusted).contains(false) && !firstSeen.contains(neighbour)
          }

          validNeighbours
        }

        val results = options.toSet
        updateMap(results, counter + 1)
        results
    }

    updateMap(results, steps)

    val debug = false
    if (debug) {
      val size        = field.width
      val min         = Coords2D(0, 0)
      val centerField = Area2D(min, Coords2D(size - 1, size - 1))
      println(
        s"Center: $centerField (${centerField.width} x ${centerField.height})"
      )
      debugPrint(centerField, firstSeen)

      Direction2D.CardinalDirections foreach { d =>
        val area = centerField + (d.diff * size)
        println(s"$d: $area (${area.width} x ${area.height})")
        debugPrint(area, firstSeen)
      }

      Direction2D.CardinalDirections foreach { d =>
        val area = centerField + (d.diff * 2 * size)
        println(s"$d $d: $area (${area.width} x ${area.height})")
        debugPrint(area, firstSeen)
      }

      Direction2D.DiagonalDirections foreach { d =>
        val area = centerField + (d.diff * size)
        println(s"$d: $area (${area.width} x ${area.height})")
        debugPrint(area, firstSeen)
      }
    }

    firstSeen.count { case (_, n) =>
      n.parity == steps.parity
    }
  }

  private def wrapCoords[T](field: Field2D[T], c: Coords2D): Coords2D = {
    val newX = absForWrappingAround(c.x, field.width)
    val newY = absForWrappingAround(c.y, field.height)
    Coords2D(newX, newY)
  }

  def part2CompareClassificationWithSimulation(
    data: Input,
    steps: Int,
  ): Long = {
    val a = part2Simulation(data, steps)
    val b = part2FieldClassification(data, steps)
    println(s"simulation = $a, classification = $b")
    a shouldEqual b
    a
  }

  // For each type of non-center field, how many fields of such type are at each "time"
  // `inProgress` is Map[time -> counts].
  // Note that for `edgeCenter` fields, the count is always `1` (which, effectively, means 4 - one for each cardinal
  // direction), but we left it like this for consistency with corner fields.
  final case class InnerCounts(
    inProgress: Map[Long, Long],
    evenCorneredFinalised: Long,
    oddCorneredFinalised: Long,
  ) {
    override def toString: String = {
      val incomplete = inProgress.toList
        .sortBy(_._1)
        .map { case (k, v) =>
          s"$v fields with $k protrusion"
        }
        .mkString(", ")

      s"$evenCorneredFinalised completed even-cornered, $oddCorneredFinalised completed odd-cornered, $incomplete"
    }
  }

  // Classify each field into categories - E, N, S, W (for "narrow cross" / "edge center") and NE, SW, SE, NW
  // (for those that flow from diagonal). They all develop similarly and we can calculate how many there are.
  //
  // Plus there is one special part1 (from center / start) field (but we already know how to count it from Part 1, so
  // we don't have to track it here).
  final case class FieldCounts(
    edgeCenter: InnerCounts,
    corner: InnerCounts,
  ) {
    override def toString: String =
      s"Edge-center: $edgeCenter\nCorner: $corner\n\n"
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
          if (completedEdgeCenter.parity == 0) {
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

        val completedEven =
          ArithmeticProgression(1, 2).S_n(coversInOneDirection.halfRoundingUp)
        val completedOdd  =
          ArithmeticProgression(2, 2).S_n(coversInOneDirection.halfRoundingDown)
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

      result
    }

    val centerSquares = countSquares(steps, distanceFromCenter, steps.parity)

    val oddSquareCount  = countSquares(steps, distanceFromCenter, 1)
    val evenSquareCount = countSquares(steps, distanceFromCenter, 0)

    println(
      s"evenSquareCount = $evenSquareCount, oddSquareCount = $oddSquareCount"
    )
    val edgeSquaresFinalised = if (steps.parity == 0) {
      fieldCounts.edgeCenter.evenCorneredFinalised * evenSquareCount + fieldCounts.edgeCenter.oddCorneredFinalised * oddSquareCount
    } else {
      fieldCounts.edgeCenter.evenCorneredFinalised * oddSquareCount + fieldCounts.edgeCenter.oddCorneredFinalised * evenSquareCount
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
      corner.oddCorneredFinalised * oddSquareCount + corner.evenCorneredFinalised * evenSquareCount
    } else {
      corner.oddCorneredFinalised * evenSquareCount + corner.evenCorneredFinalised * oddSquareCount
    }

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

    centerSquares + edgeSquaresInProgress + cornerSquaresInProgress + (edgeSquaresFinalised + cornerSquaresFinalised) * 4
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/21$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 64)}")
    println(s"Part 2: ${part2FieldClassification(realData, 26501365)}")
  }
}
