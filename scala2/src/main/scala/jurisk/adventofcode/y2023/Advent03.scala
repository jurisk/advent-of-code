package jurisk.adventofcode.y2023

import jurisk.algorithms.pathfinding.ConnectedComponents
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._

object Advent03 {
  import Square._

  sealed trait Square {
    def toChar: Char
    def isDigit: Boolean
  }

  object Square {
    final case class Digit(value: Int) extends Square {
      def toChar: Char     = ('0'.toInt + value).toChar
      def isDigit: Boolean = true
    }

    final case class Symbol(value: Char) extends Square {
      def toChar: Char     = value
      def isDigit: Boolean = false
    }

    case object Empty extends Square {
      def toChar: Char     = '.'
      def isDigit: Boolean = false
    }

    val Gear: Symbol = Symbol('*')

    def parse(ch: Char): Square =
      ch match {
        case '.'            => Empty
        case d if d.isDigit => Digit(d - '0')
        case _              => Symbol(ch)
      }
  }

  type Parsed = Field2D[Square]

  def parse(input: String): Parsed =
    Field2D.parse(input, Square.parse)

  private def extractNumbers(
    field: Field2D[Square]
  ): Set[(Int, Set[Coords2D])] = {
    val digitCoords = field.filterCoordsByValue(_.isDigit)

    val islands = ConnectedComponents
      .connectedComponents[Coords2D](
        digitCoords,
        x => {
          def helper(d: Direction2D): List[Coords2D] = {
            val n = x + d
            if (field.atOrElse(n, Empty).isDigit) n :: Nil else Nil
          }

          helper(Direction2D.W) ::: helper(Direction2D.E) ::: Nil
        },
      )

    islands.map { island =>
      val number = island.toList
        .sortBy(_.x)
        .map { c =>
          field.atOrElse(c, Empty)
        }
        .map(_.toChar)
        .mkString
        .toInt
      (number, island)
    }
  }

  def part1(data: Parsed): Int = {
    val numbers             = extractNumbers(data)
    val specialSymbolCoords = data.filterCoordsByValue {
      case Symbol(_) => true
      case _         => false
    }.toSet

    numbers.toList.collect {
      case (number, coords)
          if (coords.flatMap(
            data.adjacent8
          ) intersect specialSymbolCoords).nonEmpty =>
        number
    }.sum
  }

  def part2(data: Parsed): Int = {
    val numbers          = extractNumbers(data)
    val gearSymbolCoords = data.filterCoordsByValue(_ == Gear)

    gearSymbolCoords
      .map { gearCoordinates =>
        val catchment   = data.adjacent8(gearCoordinates).toSet
        val gearNumbers = numbers
          .collect {
            case (number, numberSquareCoordinates)
                if (catchment intersect numberSquareCoordinates).nonEmpty =>
              number
          }

        gearNumbers
      }
      .filter(_.size == 2)
      .map(_.product)
      .sum
  }

  def parseFile(fileName: String): Parsed =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Parsed = parseFile("2023/03.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
