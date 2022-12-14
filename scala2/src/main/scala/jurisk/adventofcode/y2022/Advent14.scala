package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._
import jurisk.adventofcode.y2022.Advent14.Square._
import jurisk.utils.Simulation

import scala.annotation.tailrec

object Advent14 {
  type Parsed = List[Path]

  final case class Path(
    points: List[Coords2D],
  )

  object Path {
    def parse(s: String): Path = Path {
      s.split(" -> ").toList.filter(_.nonEmpty).map(Coords2D.parse)
    }
  }

  sealed trait Square
  object Square {
    case object Empty extends Square
    case object Block extends Square
    case object Sand extends Square
  }

  def toField(parsed: Parsed): Field2D[Square] = {
    val newField = Field2D.ofSize[Square](600, 160, Square.Empty)
    parsed.foldLeft(newField) { case (acc, path) =>
      val pathSegments = path.points.sliding(2)
      pathSegments.foldLeft(acc) { case (acc2, seg) =>
        val List(from, to) = seg
        val points = Coords2D.allPointsInclusive(from, to)
        points.foldLeft(acc2) { case (acc3, point) =>
          acc3.updatedAtUnsafe(point, Block)
        }
      }
    }
  }

  @tailrec
  def nextSandGoesWhere(field: Field2D[Square], from: Coords2D): Option[Coords2D] = {
    if (from.y.value == field.height) {
      None //fail
    } else {
      val potentialDestinations = (Direction2D.S :: Direction2D.SW :: Direction2D.SE :: Nil).map { d =>
        from + d.diff
      }

      val found = potentialDestinations.find(c => field.at(c).contains(Empty))
      found match {
        case None => from.some
        case Some(found) =>
          nextSandGoesWhere(field, found)
      }
    }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Path.parse)

  def printField(field: Field2D[Square]): Unit = {
    val display = field.map {
      case Empty => '.'
      case Block => '#'
      case Sand => 'o'
    }

    println(Field2D.toDebugRepresentation(display))
  }

  def part1(data: Parsed): Int = {
    val initialField = toField(data)

    val SandOrigin = Coords2D.of(500, 0)

    Simulation.runWithIterationCount(initialField) { case (field, iteration) =>
      // printField(field)

      val sandGoes = nextSandGoesWhere(field, SandOrigin)
      println(sandGoes)

      sandGoes match {
        case None => iteration.asLeft
        case Some(c) =>
          if (c.y.value >= field.height - 1) {
            iteration.asLeft
          } else {
            field.updatedAtUnsafe(c, Sand).asRight
          }
      }
    }
  }

  def part2(data: Parsed): String =
    data.counts.toString

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/14-test.txt")
    val realData = readFileText("2022/14.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 24
    part1(real) shouldEqual 1513

    part2(test) shouldEqual 93
    part2(real) shouldEqual 123454554
  }
}
