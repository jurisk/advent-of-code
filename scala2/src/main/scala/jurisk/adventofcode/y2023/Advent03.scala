package jurisk.adventofcode.y2023

import jurisk.algorithms.pathfinding.ConnectedComponents
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._

object Advent03 {
  import Square._

  sealed trait Square {
    def toChar: Char
  }

  object Square {
    final case class Digit(value: Int) extends Square {
      override def toChar: Char = ('0'.toInt + value).toChar
    }

    final case class Symbol(value: Char) extends Square {
      override def toChar: Char = value
    }

    final case object Empty extends Square {
      override def toChar: Char = '.'
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
    Field2D.parseFromString(input, Square.parse)

  private def extractNumbers(
    field: Field2D[Square]
  ): Set[(Int, Set[Coords2D])] = {
    def isDigit(square: Square) = square match {
      case Digit(_) => true
      case _        => false
    }

    val digitCoords = field.filterCoordsByValue(isDigit)

    val islands = ConnectedComponents
      .connectedComponents[Coords2D](
        digitCoords,
        x => {
          def helper(d: Direction2D): List[Coords2D] = {
            val n = x + d
            if (field.at(n).exists(isDigit)) n :: Nil else Nil
          }

          helper(Direction2D.W) ::: helper(Direction2D.E) ::: Nil
        },
      )

    islands.map { island =>
      val number = island.toList
        .sortBy(_.x)
        .map { c =>
          field(c).toChar
        }
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
