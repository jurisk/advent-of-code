package jurisk.adventofcode.y2025

import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent09 {
  private type X = Int
  private type Y = Int

  private type Rectangle = Area2D[Int]

  type Input = Vector[Coords2D]
  sealed trait Edge {
    def valid(r: Rectangle): Boolean
  }

  private object Edge {
    final case class Left(x: X, bottomY: Y, topY: Y)   extends Edge {
      override def valid(r: Rectangle): Boolean =
        x <= r.left || x >= r.right || bottomY >= r.bottom || topY <= r.top
    }
    final case class Right(x: X, bottomY: Y, topY: Y)  extends Edge {
      override def valid(r: Rectangle): Boolean =
        x <= r.left || x >= r.right || bottomY >= r.bottom || topY <= r.top
    }
    final case class Bottom(y: Y, leftX: X, rightX: X) extends Edge {
      override def valid(r: Rectangle): Boolean =
        y <= r.top || y >= r.bottom || leftX >= r.right || rightX <= r.left
    }
    final case class Top(y: Y, leftX: X, rightX: X)    extends Edge {
      override def valid(r: Rectangle): Boolean =
        y <= r.top || y >= r.bottom || leftX >= r.right || rightX <= r.left
    }
  }
  type N = Long

  def parse(input: String): Input =
    input.parseLines(Coords2D.parse).toVector

  private def solve(data: Input, validate: Rectangle => Boolean): N =
    data
      .combinations(2)
      .map {
        case Seq(a, b) => Area2D.fromTwoPoints(a, b)
        case other     => s"Unexpected combination: $other".fail
      }
      .filter(validate)
      .map(_.areaLong)
      .max

  def part1(data: Input): N =
    solve(data, _ => true)

  private def makeEdges(input: Input): Vector[Edge] = {
    import scala.math.signum
    (input :+ input.head)
      .sliding(2)
      .map {
        case Seq(from, to) =>
          (signum(from.x - to.x), signum(from.y - to.y)) match {
            case (0, -1) => Edge.Left(from.x, from.y, to.y)
            case (0, 1)  => Edge.Right(from.x, to.y, from.y)
            case (-1, 0) => Edge.Bottom(from.y, from.x, to.x)
            case (1, 0)  => Edge.Top(from.y, to.x, from.x)
            case other   => s"Invalid edge from $from to $to: $other".fail
          }
        case other         => s"Unexpected: $other".fail
      }
      .toVector
  }

  private def isValidPart2(edges: Vector[Edge]): Rectangle => Boolean =
    r => edges.forall(_.valid(r))

  def part2(data: Input): N = {
    val edges = makeEdges(data)
    solve(data, isValidPart2(edges))
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/09$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
