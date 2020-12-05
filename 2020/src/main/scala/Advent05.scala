import scala.io.Source
import cats.effect._
import cats.implicits._

object Advent05 extends IOApp:
  opaque type ErrorMessage = String
  
  def decode(zero: Char, one: Char)(input: String): Either[ErrorMessage, Int] =
    input.map { c =>
      c match
        case `zero`   =>  0.asRight
        case `one`    =>  1.asRight
        case _        =>  ErrorMessage(s"Unexpected character $c in $input").asLeft
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
      lines           <-  IO(Source.fromResource("05.txt").getLines().toList)
      sortedSeatIds   =   lines.map(seatId).sequence.map(_.sorted)
      solution1       =   sortedSeatIds.flatMap(findLast)
      _               <-  output(solution1)
      solution2       =   sortedSeatIds.flatMap(findFree)
      _               <-  output(solution2)
    yield ExitCode.Success
