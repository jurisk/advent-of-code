package jurisk.adventofcode.y2020

import jurisk.adventofcode.y2020.Advent24.Move

import scala.annotation.tailrec
import scala.io.Source

object Advent24 extends App:
  enum Move:
    case E
    case SE
    case SW
    case W
    case NW
    case NE

  object Move:
    val All: Set[Move] = Move.values.toSet

  final case class Flip(moves: List[Move])
  final case class Coords(col: Int, row: Int):
    def applyMove(move: Move): Coords = move match
      case Move.E  => Coords(col + 1, row)
      case Move.W  => Coords(col - 1, row)

      case Move.SE => Coords(col, row + 1)
      case Move.NW => Coords(col, row - 1)

      case Move.SW => Coords(col - 1, row + 1)
      case Move.NE => Coords(col + 1, row - 1)

    def neighbours: Set[Coords] =
      Move.All.map(applyMove)

  final case class Field(black: Set[Coords] = Set.empty):
    private def interestingTiles: Set[Coords] = black ++ black.flatMap(_.neighbours)

    def applyCommands(commands: List[Flip]): Field =
      commands.foldLeft(this)((acc, flip) => acc.applyFlip(flip))

    private def applyFlip(flip: Flip): Field =
      val coords = flip.moves.foldLeft(Coords(0, 0))((acc, move) => acc.applyMove(move))
      if black.contains(coords) then Field(black - coords) else Field(black + coords)

    private def willSurvive(c: Coords): Boolean =
      val neighbouringBlacks = c.neighbours.count(black.contains)
      val weAreBlack = black.contains(c)

      (weAreBlack, neighbouringBlacks) match
        case (true, n) if n == 0 || n > 2 => false // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
        case (false, 2) => true // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
        case (x, _) => x

    private def nextDay: Field = Field(interestingTiles.filter(willSurvive))
    def days(n: Int): Field = (0 until n).foldLeft(this)((acc, _) => acc.nextDay)

  private def readCommands(fileName: String): List[Flip] =
    def parseMoves(line: List[Char]): List[Move] = line match
      case Nil => Nil
      case 'e' :: rem => Move.E :: parseMoves(rem)
      case 'w' :: rem => Move.W :: parseMoves(rem)
      case 's' :: 'e' :: rem => Move.SE :: parseMoves(rem)
      case 's' :: 'w' :: rem => Move.SW :: parseMoves(rem)
      case 'n' :: 'w' :: rem => Move.NW :: parseMoves(rem)
      case 'n' :: 'e' :: rem => Move.NE :: parseMoves(rem)
      case _ => sys.error(s"Unexpected: $line")

    Source
      .fromResource(fileName)
      .getLines()
      .map(x => Flip(parseMoves(x.toList)))
      .toList

  def run(fileName: String, expected1: Int, expected2: Int): Unit =
    val realCommands = readCommands(fileName)
    val realResult = Field().applyCommands(realCommands)
    println(realResult.black.size)
    assert(realResult.black.size == expected1)
    val realAfter100Days = realResult.days(100)
    println(realAfter100Days.black.size)
    assert(realAfter100Days.black.size == expected2)

  run("2020/24-test.txt", 10, 2208)
  run("2020/24.txt", 275, 3537)
