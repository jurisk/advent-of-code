package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxFoldableOps0
import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import jurisk.adventofcode.y2024.Advent21.DirectionalButton._
import jurisk.adventofcode.y2024.Advent21.NumericButton.InvalidNumericCoords
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Memoize
import jurisk.utils.Parsing.StringOps

object Advent21 {
  type N = Long

  private def pathBetweenCoords(
    current: Coords2D,
    toPress: Coords2D,
    invalid: Coords2D,
  ): Set[List[DirectionalButton]] = {
    def validDirections(directions: List[DirectionalButton]): Boolean =
      !directions
        .scanLeft(current) { (coords, d) =>
          coords + d.actionDiff
        }
        .contains(invalid)

    val diff = toPress - current
    val forX = if (diff.x > 0) {
      List.fill(diff.x)(Right)
    } else if (diff.x < 0) {
      List.fill(-diff.x)(Left)
    } else {
      Nil
    }
    val forY = if (diff.y > 0) {
      List.fill(diff.y)(Down)
    } else if (diff.y < 0) {
      List.fill(-diff.y)(Up)
    } else {
      Nil
    }

    Set(
      forX ++ forY ++ List(DirectionalButton.Activate),
      forY ++ forX ++ List(DirectionalButton.Activate),
    ).filter(validDirections)
  }

  sealed trait DirectionalButton extends Product with Serializable {
    def actionDiff: Coords2D
    def coords: Coords2D

    def pathTo(toPress: DirectionalButton): Set[List[DirectionalButton]] =
      pathBetweenCoords(this.coords, toPress.coords, InvalidDirectionalCoords)
  }

  object DirectionalButton {
    private val InvalidDirectionalCoords: Coords2D = Coords2D(0, 0)

    case object Up       extends DirectionalButton {
      override def actionDiff: Coords2D = Coords2D(0, -1)
      override def coords: Coords2D     = Coords2D(1, 0)
    }
    case object Down     extends DirectionalButton {
      override def actionDiff: Coords2D = Coords2D(0, 1)
      override def coords: Coords2D     = Coords2D(1, 1)
    }
    case object Left     extends DirectionalButton {
      override def actionDiff: Coords2D = Coords2D(-1, 0)
      override def coords: Coords2D     = Coords2D(0, 1)
    }
    case object Right    extends DirectionalButton {
      override def actionDiff: Coords2D = Coords2D(1, 0)
      override def coords: Coords2D     = Coords2D(2, 1)
    }
    case object Activate extends DirectionalButton {
      override def actionDiff: Coords2D = Coords2D(0, 0)
      override def coords: Coords2D     = Coords2D(2, 0)
    }

    def parseList(s: String): List[DirectionalButton] = s.map {
      case '^' => Up
      case 'v' => Down
      case '<' => Left
      case '>' => Right
      case 'A' => Activate
    }.toList
  }

  sealed trait NumericButton extends Product with Serializable {
    def coords: Coords2D
    def digit: Option[Int]

    def pathTo(toPress: NumericButton): Set[List[DirectionalButton]] =
      pathBetweenCoords(this.coords, toPress.coords, InvalidNumericCoords)
  }

  object NumericButton {
    private val InvalidNumericCoords: Coords2D = Coords2D(0, 3)

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

  private val countPairMemo: (DirectionalButton, DirectionalButton, Int) => N =
    Memoize.memoize3(countPair)

  private def countPair(
    a: DirectionalButton,
    b: DirectionalButton,
    level: Int,
  ): N =
    countPaths(a pathTo b, level - 1)

  private def countPaths(
    paths: Set[List[DirectionalButton]],
    level: Int,
  ): N =
    paths.map { path =>
      countPath(path, level)
    }.min

  private def countPath(
    presses: List[DirectionalButton],
    level: Int,
  ): N = {
    assert(presses.last == DirectionalButton.Activate)
    if (level == 0) {
      presses.length
    } else {
      (DirectionalButton.Activate :: presses).sliding2.map { case (a, b) =>
        countPairMemo(a, b, level)
      }.sum
    }
  }

  final case class Code(numericButtons: List[NumericButton]) {
    val numericPart: N =
      numericButtons.flatMap(_.digit).map(_.toString).mkString.toLong

    def complexity(robotDirectionalKeyboards: Int): N = {
      println(s"Processing for ${this.numericButtons}")
      bestHumanPressesLength(robotDirectionalKeyboards) * numericPart
    }

    def bestHumanPressesLength(
      robotDirectionalKeyboards: Int
    ): N =
      (NumericButton.Activate :: numericButtons).sliding2.map { case (a, b) =>
        countPaths(a pathTo b, robotDirectionalKeyboards)
      }.sum
  }

  object Code {
    def apply(s: String): Code =
      new Code(s.map(NumericButton.parse).toList)
  }

  type Input = List[Code]

  def parse(input: String): Input =
    input.parseLines(Code(_))

  def solve(data: Input, robotDirectionalKeyboards: Int): N =
    data.map(_.complexity(robotDirectionalKeyboards)).sum

  def part1(data: Input): N =
    solve(data, 2)

  def part2(data: Input): N =
    solve(data, 25)

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
