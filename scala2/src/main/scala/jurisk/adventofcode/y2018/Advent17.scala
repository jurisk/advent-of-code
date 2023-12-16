package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.adventofcode.y2018.Advent17.Square.Clay
import jurisk.adventofcode.y2018.Advent17.Square._
import jurisk.adventofcode.y2018.Advent17.State.consecutiveRanges
import jurisk.adventofcode.y2018.Advent17.State.isSolid
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent17 {
  sealed trait Square {
    def toChar: Char
  }

  object Square {
    case object Empty         extends Square {
      override def toChar: Char = '·'
    }
    case object Clay          extends Square {
      override def toChar: Char = '#'
    }
    case object StandingWater extends Square {
      override def toChar: Char = '~'
    }
    case object RunningWater  extends Square {
      override def toChar: Char = '|'
    }
    case object Spring        extends Square {
      override def toChar: Char = '+'
    }
  }

  final case class State(field: Field2D[Square]) {
    private def withWaterFromSpring: State =
      if (field.atOrElse(SpringOfWater.S, Empty) == Empty) {
        State(field.updatedAtUnsafe(SpringOfWater.S, RunningWater))
      } else {
        this
      }

    private def runWaterDown: State = {
      val waterSquares = field.filterCoordsByValue(_ == RunningWater)
      // Optimisation possibility: run all the way at once, not just one square
      val result       = waterSquares.foldLeft(field) { case (acc, c) =>
        acc.conditionalUpdate(c.S, _ == Empty, RunningWater)
      }
      State(result)
    }

    private def runWaterToSides: State = {
      val waterSquares = field.filterCoordsByValue(_ == RunningWater)
      val result       = waterSquares.foldLeft(field) { case (acc, c) =>
        if (isSolid(acc, c.S)) {
          // Optimisation possibility: all the way at once, not just one square
          // Optimisation possibility: do a check for pools you could stabilize here
          acc
            .conditionalUpdate(c.W, _ == Empty, RunningWater)
            .conditionalUpdate(c.E, _ == Empty, RunningWater)
        } else {
          acc
        }
      }
      State(result)
    }

    private def findCandidatePools: List[(Coords2D, Coords2D)] =
      field.yIndices.flatMap { y =>
        consecutiveRanges(field.row(y), RunningWater) map { case (start, end) =>
          (
            Coords2D(field.topLeft.x + start, y),
            Coords2D(field.topLeft.x + end, y),
          )
        }
      }.toList

    // Optimisation possibility - do this only when you expand water to the side, and not otherwise
    private def stabilisePools: State = {
      val candidateWaterPoints = findCandidatePools
      val result               = candidateWaterPoints.foldLeft(field) {
        case (acc, (from, to)) =>
          val isSolidBasement = Coords2D
            .allPointsInclusive(from.S, to.S)
            .forall(c => isSolid(acc, c))
          val hasLeftSide     = acc.atOrElse(from.W, Empty) == Clay
          val hasRightSide    = acc.atOrElse(to.E, Empty) == Clay

          if (isSolidBasement && hasLeftSide && hasRightSide) {
            Coords2D.allPointsInclusive(from, to).foldLeft(acc) {
              case (nestedAcc, c) =>
                nestedAcc.conditionalUpdate(c, _ == RunningWater, StandingWater)
            }
          } else acc
      }
      State(result)
    }

    def next: State =
      withWaterFromSpring.runWaterDown.runWaterToSides.stabilisePools
  }

  object State {
    private def isSolid(field: Field2D[Square], c: Coords2D): Boolean =
      field.atOrElse(c, Empty) match {
        case Clay          => true
        case StandingWater => true
        case _             => false
      }

    @tailrec
    private def consecutiveRanges[T](
      vector: Vector[T],
      searchValue: T,
      offset: Int = 0,
      acc: List[(Int, Int)] = Nil,
    ): List[(Int, Int)] = {
      val start = vector.indexOf(searchValue)
      if (start == -1) {
        acc
      } else {
        val taken = vector.drop(start).takeWhile(_ == searchValue).length
        val added = (offset + start, offset + start + taken - 1)
        consecutiveRanges(
          vector.drop(start + taken),
          searchValue,
          offset + start + taken,
          added :: acc,
        )
      }
    }
  }

  private def parsePoints(s: String): List[Coords2D] =
    s match {
      case s"x=$x, y=$y1..$y2" =>
        Coords2D.allPointsInclusive(
          Coords2D.of(x.toInt, y1.toInt),
          Coords2D.of(x.toInt, y2.toInt),
        )
      case s"y=$y, x=$x1..$x2" =>
        Coords2D.allPointsInclusive(
          Coords2D.of(x1.toInt, y.toInt),
          Coords2D.of(x2.toInt, y.toInt),
        )
      case _                   => s.failedToParse
    }

  private val SpringOfWater: Coords2D = Coords2D.of(500, 0)

  def parse(data: String): Field2D[Square] = {
    val points = data.parseLines(parsePoints).flatten

    val boundingBox = Coords2D
      .boundingBoxInclusive(SpringOfWater :: points)
      .expandInEachDirectionBy(1)

    val field = Field2D
      .forArea[Square](boundingBox, Empty)
      .updatedAtUnsafe(SpringOfWater, Spring)

    points.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c, Clay)
    }
  }

  private def printField(name: String, field: Field2D[Square]): Unit =
    Field2D.printField[Square](field, _.toChar, name.some)

  def simulate(field: Field2D[Square]): Field2D[Square] = {
    printField("Before:", field)

    val (result, _) = Simulation.runUntilStableStateWithCounter(State(field)) {
      case (state, iteration) =>
        val newState = state.next
        if (iteration % 1_000 == 0) {
          printField("Next", newState.field)
        }
        newState
    }

    printField("After:", result.field)

    result.field
  }

  private def squareCount(
    field: Field2D[Square],
    lookingFor: Set[Square],
  ): Int = {
    val yCoords = field.entries.flatMap { case (c, v) =>
      if (v == Clay) {
        c.y.some
      } else {
        none
      }
    }
    val minY    = yCoords.min
    val maxY    = yCoords.max

    field.entries.map { case (c, v) =>
      if ((c.y >= minY) && (c.y <= maxY) && lookingFor.contains(v)) {
        1
      } else {
        0
      }
    }.sum
  }

  def part1(field: Field2D[Square]): Int =
    squareCount(field, Set(StandingWater, RunningWater))

  def part2(field: Field2D[Square]): Int =
    squareCount(field, Set(StandingWater))

  def parseFile(fileName: String): Field2D[Square] = parse(
    readFileText(fileName)
  )

  def main(args: Array[String]): Unit = {
    val realData = parseFile("2018/17.txt")
    val testData = parseFile("2018/17-test.txt")

    val testResult = simulate(testData)
    part1(testResult) shouldEqual 57
    part2(testResult) shouldEqual 29

    val realResult = simulate(realData)
    part1(realResult) shouldEqual 33004
    part2(realResult) shouldEqual 23294
  }
}
