package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._
import jurisk.adventofcode.y2022.Advent14.Square._
import jurisk.utils.Simulation

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
    val width = 2000

    val maxY   = parsed.flatMap(_.points).map(_.y.value).max
    val height = maxY + 3

    val newField = Field2D.ofSize[Square](width, height, Square.Empty)
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
      (0 until width).foldLeft(result) { case (acc, x) =>
        acc.updatedAtUnsafe(Coords2D.of(x, height - 1), Block)
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
        .map(from + _.diff)

    val found = potentialDestinations.find(c => field.at(c).contains(Empty))
    found match {
      case None        => from
      case Some(found) =>
        nextSandGoesWhere(field, found)
    }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Path.parse)

  private def printField(field: Field2D[Square]): Unit = {
    val display = field.map {
      case Empty => '.'
      case Block => '#'
      case Sand  => 'o'
    }

    println(Field2D.toDebugRepresentation(display))
  }

  private val SandOrigin: Coords2D = Coords2D.of(500, 0)

  private def sandSimulation(
    field: Field2D[Square],
    terminationClause: Coords2D => Boolean,
  ): Int =
    Simulation.runWithIterationCount(field) { case (field, iteration) =>
      val sandGoes = nextSandGoesWhere(field, SandOrigin)

      if (terminationClause(sandGoes)) {
        printField(field)
        iteration.asLeft
      } else {
        field.updatedAtUnsafe(sandGoes, Sand).asRight
      }
    }

  def part1(data: Parsed): Int = {
    val field = toField(data, rockBounded = false)
    sandSimulation(field, _.y.value >= field.height - 1)
  }

  def part2(data: Parsed): Int = {
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
