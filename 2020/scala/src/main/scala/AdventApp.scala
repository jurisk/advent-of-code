import cats.effect.{ExitCode, IO, IOApp}
import cats.Show
import scala.io.Source
import cats.implicits._
import AdventApp.ErrorMessage

object AdventApp:
  opaque type ErrorMessage = String
  object ErrorMessage:
    def apply(message: String): ErrorMessage = message

sealed private trait AdventApp[TestCase, Output] extends IOApp:
  def fileName: String
  
  def parseTestCases(lines: List[String]): Either[ErrorMessage, List[TestCase]]
  
  def solution1(input: List[TestCase]): Output
  def solution2(input: List[TestCase]): Output
  
  def run(args: List[String]): IO[ExitCode] = {
    def printOutput(answer: Either[ErrorMessage, Output]): IO[Unit] =
      IO(println(answer.bimap(x => s"Error: $x", _.toString).merge))
    
    for
      lines           <-  IO(Source.fromResource(fileName).getLines().toList)
      testCases       =   parseTestCases(lines)
      answer1         =   testCases.map(solution1)
      _               <-  printOutput(answer1)
      answer2         =   testCases.map(solution2)
      _               <-  printOutput(answer2)
    yield ExitCode.Success
  }

trait SingleLineAdventApp[TestCase, Output] extends AdventApp[TestCase, Output]:
  def parseLine(line: String): Either[ErrorMessage, TestCase]

  def parseTestCases(lines: List[String]): Either[ErrorMessage, List[TestCase]] = 
    (lines map parseLine).sequence

trait MultiLineAdventApp[TestCase, Output] extends AdventApp[TestCase, Output]:
  def parseLines(line: List[String]): Either[ErrorMessage, TestCase]

  def parseTestCases(lines: List[String]): Either[ErrorMessage, List[TestCase]] =
    lines
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toList)
      .map(parseLines)
      .toList
      .sequence
