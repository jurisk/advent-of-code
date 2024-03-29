package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.adventofcode.y2022.Advent14.Square._
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent14 {
  type Parsed = List[Path]

  final case class Path(
    points: List[Coords2D]
  )

  object Path {
    def parse(s: String): Path = Path {
      s.split(" -> ").toList.map(Coords2D.parse)
    }
  }

  sealed trait Square
  object Square {
    case object Empty extends Square
    case object Block extends Square
    case object Sand  extends Square
  }

  private def toField(parsed: Parsed, rockBounded: Boolean): Field2D[Square] = {
    val minX = parsed.flatMap(_.points).map(_.x).min

    val maxY   = parsed.flatMap(_.points).map(_.y).max
    val height = maxY + 3
    val width  = height * 3

    val newField = Field2D.ofSize[Square](
      width,
      height,
      Square.Empty,
      topLeft = Coords2D.of(minX - height, 0),
    )
    val result   = parsed.foldLeft(newField) { case (acc1, path) =>
      val pathSegments = path.points.sliding(2)
      pathSegments.foldLeft(acc1) { case (acc2, seg) =>
        val List(from, to) = seg
        val points         = Coords2D.allPointsInclusive(from, to)
        points.foldLeft(acc2) { case (acc3, point) =>
          acc3.updatedAtUnsafe(point, Block)
        }
      }
    }

    if (rockBounded) {
      result.coordsForRow(height - 1).foldLeft(result) { case (acc, c) =>
        acc.updatedAtUnsafe(c, Block)
      }
    } else {
      result
    }
  }

  @tailrec
  private def nextSandGoesWhere(
    field: Field2D[Square],
    from: Coords2D,
  ): Coords2D = {
    val potentialDestinations =
      (Direction2D.S :: Direction2D.SW :: Direction2D.SE :: Nil)
        .map(from + _)

    // The .at is important, cannot default to Empty
    val found = potentialDestinations.find(c => field.at(c).contains(Empty))
    found match {
      case None        => from
      case Some(found) =>
        nextSandGoesWhere(field, found)
    }
  }

  def parse(data: String): Parsed =
    data.parseLines(Path.parse)

  private def printField(field: Field2D[Square]): Unit = {
    val display = field.map {
      case Empty => '.'
      case Block => '#'
      case Sand  => 'o'
    }

    Field2D.printCharField(display)
  }

  private val SandOrigin: Coords2D = Coords2D.of(500, 0)

  private def sandSimulation(
    field: Field2D[Square],
    terminationClause: Coords2D => Boolean,
  ): Long =
    Simulation.runWithIterationCount(field) { case (field, iteration) =>
      val sandGoes = nextSandGoesWhere(field, SandOrigin)

      if (terminationClause(sandGoes)) {
        printField(field)
        iteration.asLeft
      } else {
        field.updatedAtUnsafe(sandGoes, Sand).asRight
      }
    }

  def part1(data: Parsed): Long = {
    val field = toField(data, rockBounded = false)
    sandSimulation(field, _.y >= field.height - 1)
  }

  def part2(data: Parsed): Long = {
    val field = toField(data, rockBounded = true)
    sandSimulation(field, _ == SandOrigin) + 1
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/14-test.txt")
    val realData = readFileText("2022/14.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 24
    part1(real) shouldEqual 1513

    part2(test) shouldEqual 93
    part2(real) shouldEqual 22646
  }
}
