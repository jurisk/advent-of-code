package jurisk.adventofcode.y2024

import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.adventofcode.y2024.Advent21.DirectionalButton._
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent21 {
  type N = Long

  sealed trait DirectionalButton
  object DirectionalButton {
    case object Up       extends DirectionalButton
    case object Down     extends DirectionalButton
    case object Left     extends DirectionalButton
    case object Right    extends DirectionalButton
    case object Activate extends DirectionalButton

    def parseList(s: String): List[DirectionalButton] = s.map {
      case '^' => Up
      case 'v' => Down
      case '<' => Left
      case '>' => Right
      case 'A' => Activate
    }.toList
  }

  def toPressNumericButton(
    current: NumericButton,
    toPress: NumericButton,
  ): Set[List[DirectionalButton]] = {
    val diff = toPress.coords - current.coords
    val forX = if (diff.x > 0) {
      List.fill(diff.x)(Right)
    } else if (diff.x < 0) {
      List.fill(-diff.x)(Left)
    } else {
      Nil
    }
    val forY = if (diff.y > 0) {
      List.fill(diff.y)(Up)
    } else if (diff.y < 0) {
      List.fill(-diff.y)(Down)
    } else {
      Nil
    }

    // TODO: Need to avoid visiting the illegal gap!

    Set(
      forX ++ forY ++ List(DirectionalButton.Activate),
      forY ++ forX ++ List(DirectionalButton.Activate),
    )
  }

  sealed trait NumericButton extends Product with Serializable {
    def coords: Coords2D
    def digit: Option[Int]
  }
  object NumericButton {
    val All: Set[NumericButton]                =
      Set(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Activate)
    val ByCoords: Map[Coords2D, NumericButton] =
      All.map(nb => nb.coords -> nb).toMap

    case object Zero     extends NumericButton {
      override def coords: Coords2D   = Coords2D(1, 3)
      override def digit: Option[Int] = 0.some
    }
    case object One      extends NumericButton {
      override def coords: Coords2D   = Coords2D(0, 2)
      override def digit: Option[Int] = 1.some
    }
    case object Two      extends NumericButton {
      override def coords: Coords2D   = Coords2D(1, 2)
      override def digit: Option[Int] = 2.some
    }
    case object Three    extends NumericButton {
      override def coords: Coords2D   = Coords2D(2, 2)
      override def digit: Option[Int] = 3.some
    }
    case object Four     extends NumericButton {
      override def coords: Coords2D   = Coords2D(0, 1)
      override def digit: Option[Int] = 4.some
    }
    case object Five     extends NumericButton {
      override def coords: Coords2D   = Coords2D(1, 1)
      override def digit: Option[Int] = 5.some
    }
    case object Six      extends NumericButton {
      override def coords: Coords2D   = Coords2D(2, 1)
      override def digit: Option[Int] = 6.some
    }
    case object Seven    extends NumericButton {
      override def coords: Coords2D   = Coords2D(0, 0)
      override def digit: Option[Int] = 7.some
    }
    case object Eight    extends NumericButton {
      override def coords: Coords2D   = Coords2D(1, 0)
      override def digit: Option[Int] = 8.some
    }
    case object Nine     extends NumericButton {
      override def coords: Coords2D   = Coords2D(2, 0)
      override def digit: Option[Int] = 9.some
    }
    case object Activate extends NumericButton {
      override def coords: Coords2D   = Coords2D(2, 3)
      override def digit: Option[Int] = none
    }

    def parse(ch: Char): NumericButton = ch match {
      case '0' => Zero
      case '1' => One
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case 'A' => Activate
      case _   => "Invalid".fail
    }
  }

  final case class Code(numericButtons: List[NumericButton]) {
    def humanPresses: List[DirectionalButton] = Nil
    def numericPart: N                        =
      numericButtons.flatMap(_.digit.toString).mkString.toLong

    def complexity: N = humanPresses.length * numericPart

    def firstLevelPresses(
      current: NumericButton = NumericButton.Activate
    ): Set[List[DirectionalButton]] =
      numericButtons match {
        case h :: t =>
          val result = toPressNumericButton(current, h)
          t.foldLeft(result) { (acc, nb) =>
            acc.flatMap { presses =>
              toPressNumericButton(h, nb).map(presses ++ _)
            }
          }
        case Nil    =>
          Set(Nil)
      }
  }

  object Code {
    def apply(s: String): Code =
      new Code(s.map(NumericButton.parse).toList)
  }

  type Input = List[Code]

  def parse(input: String): Input =
    input.parseLines(Code(_))

  def part1(data: Input): N =
    data.map(_.complexity).sum

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
