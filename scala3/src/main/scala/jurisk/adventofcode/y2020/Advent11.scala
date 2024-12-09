package jurisk.adventofcode.y2020

import cats.implicits.*
import AdventApp.ErrorMessage
import jurisk.adventofcode.y2020.Advent11.{Line, Seat}

import scala.annotation.tailrec

object Advent11 extends SingleLineAdventApp[Line, Int]:
  def fileName: String = "11"

  enum Seat:
    case Floor
    case Empty
    case Occupied

  type Line = List[Seat]
  
  val directionOffsets: List[(Int, Int)] = (
    for
      r <- -1 to 1
      c <- -1 to 1
      if r != 0 || c != 0
    yield (r, c)
  ).toList
  
  type ConversionFunction = (List[Line], Int, Int, Seat) => Seat
  
  def seatAt(input: List[Line], r: Int, c: Int): Option[Seat] =
    input.lift(r).flatMap(_.lift(c))   
  
  def convertSeat1(input: List[Line], rowIdx: Int, colIdx: Int, seat: Seat): Seat =
    val neighbours: List[Seat] = directionOffsets.flatMap { case (r, c) =>
      seatAt(input, r + rowIdx, c + colIdx)
    }

    val occupiedNeighbours = neighbours.count(_ == Seat.Occupied)
    
    seat match
      case Seat.Empty if occupiedNeighbours == 0 => Seat.Occupied
      case Seat.Occupied if occupiedNeighbours >= 4 => Seat.Empty
      case x => x

  def convertSeat2(input: List[Line], rowIdx: Int, colIdx: Int, seat: Seat): Seat =
    @tailrec
    def seeOccupied(r: Int, c: Int, dr: Int, dc: Int): Boolean =
      seatAt(input, r + dr, c + dc) match
        case None => false
        case Some(Seat.Occupied) => true
        case Some(Seat.Empty) => false
        case Some(Seat.Floor) => seeOccupied(r + dr, c + dc, dr, dc)
    
    val visibleOccupied = directionOffsets.count((r, c) => seeOccupied(rowIdx, colIdx, r, c))
    
    seat match
      case Seat.Empty if visibleOccupied == 0 => Seat.Occupied
      case Seat.Occupied if visibleOccupied >= 5 => Seat.Empty
      case x => x

  def next(input: List[Line], conversionFunction: ConversionFunction): List[Line] =
    input.zipWithIndex.map { case (row, rowIdx) =>
      row.zipWithIndex.map { case (seat, colIdx) =>
        conversionFunction(input, rowIdx, colIdx, seat)
      }
    }

  def debugRender(data: List[Line]): String =
    data.map(
      _.map {
        case Seat.Empty    => 'L'
        case Seat.Occupied => '#'
        case Seat.Floor    => '.'
      }.mkString
    ).mkString("\n")
  
  @tailrec
  def solve(input: List[Line], conversionFunction: ConversionFunction): Int =
    val obtained = next(input, conversionFunction)

    if input == obtained then
      input.flatten.count {
        case Seat.Occupied => true
        case _ => false
      }
    else
      solve(obtained, conversionFunction)

  def solution1(input: List[Line]): Int =
    solve(input, convertSeat1)

  def solution2(input: List[Line]): Int =
    solve(input, convertSeat2)
  
  def parseLine(line: String): Either[ErrorMessage, Line] =
    line
      .map {
        case '.' => Seat.Floor.asRight
        case 'L' => Seat.Empty.asRight
        case '#' => Seat.Occupied.asRight
        case x   => ErrorMessage(s"Unrecognized $x").asLeft
      }
      .toList
      .sequence
