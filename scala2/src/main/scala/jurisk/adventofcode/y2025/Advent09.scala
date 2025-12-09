package jurisk.adventofcode.y2025

import jurisk.collections.immutable.SetOfTwo
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent09 {
  private type X = Int
  private type Y = Int

  private type Rectangle = SetOfTwo[Coords2D]

  type Input = List[Coords2D]
  sealed trait Edge {
    def valid(rectangle: Rectangle): Boolean
  }

  private object Edge {
    final case class Left(x: X, bottomY: Y, topY: Y)   extends Edge {
      override def valid(rectangle: Rectangle): Boolean = ???
    }
    final case class Right(x: X, bottomY: Y, topY: Y)  extends Edge {
      override def valid(rectangle: Rectangle): Boolean = ???
    }
    final case class Bottom(y: Y, leftX: X, rightX: X) extends Edge {
      override def valid(rectangle: Rectangle): Boolean = ???
    }
    final case class Top(y: Y, leftX: X, rightX: X)    extends Edge {
      override def valid(rectangle: Rectangle): Boolean = ???
    }
  }
  type N = Long

  def parse(input: String): Input =
    input.parseLines(Coords2D.parse)

  private def rectangleSize(rectangle: Rectangle): N = {
    val (a, b) = rectangle.tupleInArbitraryOrder
    val width  = (a.x - b.x).abs + 1
    val height = (a.y - b.y).abs + 1
    width.toLong * height.toLong
  }

  private def solve(data: Input, validate: Rectangle => Boolean): N =
    data
      .combinations(2)
      .map {
        case List(a, b) =>
          SetOfTwo(a, b)
        case other      =>
          s"Unexpected combination: $other".fail
      }
      .filter {
        validate
      }
      .map {
        rectangleSize
      }
      .max

  def part1(data: Input): N =
    solve(data, _ => true)

  private def makeEdges(input: Input): List[Edge] =
    ???

  private def isValidPart2(segments: List[Edge]): Rectangle => Boolean = { r =>
    segments forall { s =>
      s.valid(r)
    }
  }

  def part2(data: Input): N =
    solve(data, isValidPart2(makeEdges(data)))

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
