package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.adventofcode.y2022.Advent17.Square.Empty
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.Utils.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent17 {
  sealed trait JetMove {
    def toChar: Char = this match {
      case JetMove.Left  => '>'
      case JetMove.Right => '<'
    }

    override def toString: String = this match {
      case JetMove.Left  => "Left"
      case JetMove.Right => "Right"
    }

    def diff: Coords2D = this match {
      case JetMove.Left  => Direction2D.W.diff
      case JetMove.Right => Direction2D.E.diff
    }
  }
  object JetMove       {
    object Left  extends JetMove
    object Right extends JetMove
  }

  type JetPattern = Vector[JetMove]
  def parse(data: String): JetPattern =
    data.map {
      case '>' => JetMove.Right
      case '<' => JetMove.Left
    }.toVector

  sealed trait Square
  object Square {
    case object Wall  extends Square
    case object Rock  extends Square
    case object Empty extends Square
  }

  final case class RockPattern(coords: Set[Coords2D])
  object RockPattern {
    import Coords2D.Zero

    val Horizontal = RockPattern(
      Set(
        Zero,
        Zero.E,
        Zero.E.E,
        Zero.E.E.E,
      )
    )
    val Cross      = RockPattern(
      Set(
        Zero.N,
        Zero.E,
        Zero.NE,
        Zero.NE.N,
        Zero.NE.E,
      )
    )
    val ReverseL   = RockPattern(
      Set(
        Zero,
        Zero.E,
        Zero.E.E,
        Zero.E.NE,
        Zero.NE.NE,
      )
    )
    val Vertical   = RockPattern(
      Set(
        Zero,
        Zero.N,
        Zero.N.N,
        Zero.N.N.N,
      )
    )
    val Block      = RockPattern(
      Set(
        Zero,
        Zero.N,
        Zero.E,
        Zero.NE,
      )
    )

    val RockPatterns: Array[RockPattern] =
      Array(Horizontal, Cross, ReverseL, Vertical, Block)
  }

  def hitsFieldOrSides(
    field: Field2D[Square],
    rockCoords: Set[Coords2D],
  ): Boolean =
    !rockCoords.forall(c => field.at(c).contains(Empty))

  def fieldMinY(field: Field2D[Square]): Int =
    field
      .filterCoordsByValue {
        case Square.Wall  => true
        case Square.Rock  => true
        case Square.Empty => false
      }
      .map(_.y.value)
      .min

  private def printField(
    field: Field2D[Square],
    rockCoords: Set[Coords2D],
  ): Unit = {
    val adjusted = rockCoords.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c, Square.Rock)
    }

    val charField = adjusted.map {
      case Square.Wall  => '#'
      case Square.Rock  => '@'
      case Square.Empty => '.'
    }

    val asString = Field2D.toDebugRepresentation(charField)
    println(asString)
    println
  }

  val Debug = false
  private def dropRock(
    field: Field2D[Square],
    jetPattern: JetPattern,
    rock: RockPattern,
    jetPatternOffset: Int,
  ): (Field2D[Square], Int) = {
    val minY                    = fieldMinY(field) // should perhaps cache
    val startPosition: Coords2D = Coords2D.of(2, minY - 4)
    val rockCoords              = rock.coords.map(_ + startPosition)

    val (restingCoords, newOffset) =
      Simulation.runWithIterationCount(rockCoords) {
        case (rockCoords, iteration) =>
          if (Debug) {
            println("The rock starts falling")
            printField(field, rockCoords)
          }

          val totalJetPatternOffset = jetPatternOffset + iteration
          if (Debug) {
            println(s"Offset in jet pattern: $totalJetPatternOffset")
          }
          val nextMove              = jetPattern(totalJetPatternOffset % jetPattern.length)

          val movedByJetStream = rockCoords.map(_ + nextMove.diff)
          val adjusted         = if (hitsFieldOrSides(field, movedByJetStream)) {
            if (Debug) {
              println(
                s"Jet of gas pushes rock ${nextMove.toString}, but nothing happens"
              )
              printField(field, rockCoords)
            }

            rockCoords
          } else {
            if (Debug) {
              println(s"Jet of gas pushes rock ${nextMove.toString}")
              printField(field, movedByJetStream)
            }

            movedByJetStream
          }

          val ifGoesDown = adjusted.map(_.S)
          if (hitsFieldOrSides(field, ifGoesDown)) {
            if (Debug) {
              println(s"Rock falls 1 unit, causing it to come to rest")
              printField(field, adjusted)
            }
            (adjusted, totalJetPatternOffset + 1).asLeft
          } else {
            if (Debug) {
              println(s"Rock falls 1 unit")
              printField(field, ifGoesDown)
            }
            ifGoesDown.asRight
          }
      }

    val result = restingCoords.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c, Square.Wall)
    }

    (result, newOffset)
  }

  def part1(data: JetPattern, rocksToCount: Long): Long = {
    val emptyField: Field2D[Square] = Field2D.ofSize(7, 4000, Square.Empty)
    val withWalls: Field2D[Square]  = emptyField.mapByCoords { c =>
      if (c.y.value == emptyField.height - 1) { // (c.x.value == 0) || (c.x.value == emptyField.width - 1)
        Square.Wall
      } else {
        Square.Empty
      }
    }

    val result = Simulation.runWithIterationCount((withWalls, 0)) {
      case (state, iteration) =>
        val (acc, jetPatternOffset) = state
        if (iteration >= rocksToCount) {
          acc.asLeft
        } else {
          println(iteration)
          val chosenRock                    = RockPattern.RockPatterns(
            iteration % RockPattern.RockPatterns.length
          )
          val (result, newJetPatternOffset) =
            dropRock(acc, data, chosenRock, jetPatternOffset)
          (result, newJetPatternOffset).asRight
        }
    }

    // tower height
    result.height - fieldMinY(result) - 1
  }

  def part2(data: JetPattern): String =
    data.counts.toString

  def main(args: Array[String]): Unit = {
    val testData = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""
    val realData = readFileText("2022/17.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test, 2022) shouldEqual 3068
    part1(real, 2022) shouldEqual 3071

    part1(test, 1000000000000L) shouldEqual 1514285714288L
    part1(real, 1000000000000L) shouldEqual "todo"
  }
}
