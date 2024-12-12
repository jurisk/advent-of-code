package jurisk.adventofcode

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import jurisk.adventofcode.AdventApp.ErrorMessage

import scala.io.Source

object AdventApp:
  opaque type ErrorMessage = String
  object ErrorMessage:
    def apply(message: String): ErrorMessage = message

sealed private trait AdventApp[TestCase, Output] extends IOApp:
  def year: Int
  def exercise: Int

  private def makeFilePath(suffix: Option[String]): String = f"$year/$exercise%02d${suffix.fold("")(s => s"-$s")}.txt"

  def parseTestCases(lines: List[String]): Either[ErrorMessage, List[TestCase]]

  def solution1(input: List[TestCase]): Output
  def solution2(input: List[TestCase]): Output

  def parseRealData: IO[Either[ErrorMessage, List[TestCase]]] =
    parseData(makeFilePath(none))

  def parseTestData(suffix: String): IO[Either[ErrorMessage, List[TestCase]]] =
    parseData(makeFilePath(s"test-$suffix".some))

  def parseData(path: String): IO[Either[ErrorMessage, List[TestCase]]] =
    for
      lines <- IO(Source.fromResource(path).getLines().toList)
      testCases = parseTestCases(lines)
    yield testCases

  def run(args: List[String]): IO[ExitCode] = {
    def printOutput(answer: Either[ErrorMessage, Output]): IO[Unit] =
      IO(println(answer.bimap(x => s"Error: $x", _.toString).merge))

    for
      testCases       <-  parseRealData
      answer1         =   testCases map solution1
      _               <-  printOutput(answer1)
      answer2         =   testCases map solution2
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
