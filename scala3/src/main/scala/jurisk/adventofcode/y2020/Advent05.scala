package jurisk.adventofcode.y2020

import cats.effect.*
import cats.implicits.*
import jurisk.adventofcode.AdventApp.ErrorMessage

import scala.io.Source

object Advent05 extends IOApp:
  def decode(zero: Char, one: Char)(input: String): Either[ErrorMessage, Int] =
    input.map {
        case `zero` => 0.asRight
        case `one` => 1.asRight
        case c => ErrorMessage(s"Unexpected character $c in $input").asLeft
      }
    .toList
    .sequence
    .map(_.foldLeft(0) { case (acc, x) => acc * 2 + x})

  def seatId(x: String): Either[ErrorMessage, Int] =
    for
      encoded                     <-  if x.length == 10
                                        then x.splitAt(7).asRight
                                        else ErrorMessage(s"Invalid input $x").asLeft
      (encodedRow, encodedColumn) =   encoded
      row                         <-  decode('F', 'B')(encodedRow)
      column                      <-  decode('L', 'R')(encodedColumn)
    yield row * 8 + column

  def findLast(sortedSeatIds: List[Int]): Either[ErrorMessage, Int] =
    sortedSeatIds.lastOption.toRight(ErrorMessage("List was empty"))

  def findFree(sortedSeatIds: List[Int]): Either[ErrorMessage, Int] =
    (sortedSeatIds.init zip sortedSeatIds.tail)
      .find { case (a, b) =>
        b - a == 2
      }
      .map { case (a, _) =>
        a + 1
      }
      .toRight(ErrorMessage(s"Didn't find a free seat in $sortedSeatIds"))

  def output(x: Either[ErrorMessage, Int]): IO[Unit] =
    IO(println(x.fold(x => s"Error: $x", x => s"$x")))

  def run(args: List[String]): IO[ExitCode] =
    for
      lines           <-  IO(Source.fromResource("2020/05.txt").getLines().toList)
      sortedSeatIds   =   lines.map(seatId).sequence.map(_.sorted)
      solution1       =   sortedSeatIds.flatMap(findLast)
      _               <-  output(solution1)
      solution2       =   sortedSeatIds.flatMap(findFree)
      _               <-  output(solution2)
    yield ExitCode.Success
