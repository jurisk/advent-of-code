package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.algorithms.pathfinding.Pathfinding
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent12 {
  final case class Task(
    field: Field2D[Elevation],
    start: Coords2D,
    end: Coords2D,
  )

  final case class Elevation private (value: Char) extends AnyVal {
    private def intHeight: Int             = value - 'a'
    def canGoTo(other: Elevation): Boolean = (other.intHeight - intHeight) <= 1
  }

  object Elevation {
    def apply(ch: Char): Elevation =
      if ((ch >= 'a') && (ch <= 'z')) new Elevation(ch)
      else s"Bad character $ch".fail

    val Lowest: Elevation  = Elevation('a')
    val Highest: Elevation = Elevation('z')
  }

  def parse(data: String): Task = {
    val charField   = Field2D.parseCharField(data)
    val List(start) = charField.filterCoordsByValue(_ == 'S')
    val List(end)   = charField.filterCoordsByValue(_ == 'E')

    val field = charField map {
      case 'S' => Elevation.Lowest
      case 'E' => Elevation.Highest
      case ch  => Elevation(ch)
    }

    Task(
      field = field,
      start = start,
      end = end,
    )
  }

  def part1(task: Task): Option[Int] =
    Pathfinding.shortestPathLength[Coords2D](
      task.start,
      task.field.createSuccessorsFunction(
        (a, b) => a canGoTo b,
        includeDiagonal = false,
      ),
      _ == task.end,
    )

  def part2(task: Task): Option[Int] =
    Pathfinding.shortestPathLength[Coords2D](
      task.end,
      task.field.createSuccessorsFunction(
        (a, b) => b canGoTo a,
        includeDiagonal = false,
      ),
      c => task.field.at(c).contains(Elevation.Lowest),
    )

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/12-test.txt")
    val realData = readFileText("2022/12.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual Some(31)
    part1(real) shouldEqual Some(412)

    part2(test) shouldEqual Some(29)
    part2(real) shouldEqual Some(402)
  }
}
