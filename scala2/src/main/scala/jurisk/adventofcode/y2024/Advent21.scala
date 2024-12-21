package jurisk.adventofcode.y2024

import jurisk.adventofcode.y2024.Advent21.DirectionalButton._
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent21 {
  type N     = Long

  sealed trait DirectionalButton
  object DirectionalButton {
    case object Up extends DirectionalButton
    case object Down extends DirectionalButton
    case object Left extends DirectionalButton
    case object Right extends DirectionalButton
    case object Activate extends DirectionalButton
  }

  def toPressNumericButton(current: NumericButton, toPress: NumericButton): Set[List[DirectionalButton]] = {
    val diff = toPress.coords - current.coords
    var results = Set.empty

    results
  }

  sealed trait NumericButton extends Product with Serializable {
    def coords: Coords2D
  }
  object NumericButton {
    val All: Set[NumericButton] = Set(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Activate)
    val ByCoords: Map[Coords2D, NumericButton] = All.map(nb => nb.coords -> nb).toMap

    case object Zero extends NumericButton {
      override def coords: Coords2D = Coords2D(1, 3)
    }
    case object One extends NumericButton {
      override def coords: Coords2D = Coords2D(0, 2)
    }
    case object Two extends NumericButton {
      override def coords: Coords2D = Coords2D(1, 2)
    }
    case object Three extends NumericButton {
        override def coords: Coords2D = Coords2D(2, 2)
    }
    case object Four extends NumericButton {
        override def coords: Coords2D = Coords2D(0, 1)
    }
    case object Five extends NumericButton {
      override def coords: Coords2D = Coords2D(1, 1)
    }
    case object Six extends NumericButton {
      override def coords: Coords2D = Coords2D(2, 1)
    }
    case object Seven extends NumericButton {
      override def coords: Coords2D = Coords2D(0, 0)
    }
    case object Eight extends NumericButton {
      override def coords: Coords2D = Coords2D(1, 0)
    }
    case object Nine extends NumericButton {
        override def coords: Coords2D = Coords2D(2, 0)
    }
    case object Activate extends NumericButton {
        override def coords: Coords2D = Coords2D(2, 3)
    }
  }

  final case class Code(chars: String) {
    def humanPresses: List[DirectionalButton] = Nil
    def numericPart: N = chars.filter(_.isDigit).mkString.toLong

    def complexity: N = humanPresses.length * numericPart
  }

  type Input = List[Code]

  def parse(input: String): Input =
    input.parseLines(Code(_))

  def part1(data: Input): N = {
    data.map(_.complexity).sum
  }

  def part2(data: Input): N =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/21$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
