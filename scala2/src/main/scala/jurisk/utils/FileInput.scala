package jurisk.utils

import jurisk.utils.Utils.IterableOps

import scala.io.Source
import scala.util.Using

object FileInput {
  def parseSingleFileLine[T](fileName: String, parser: Char => T): List[T] =
    readSingleFileLine(fileName).map(parser).toList

  def readSingleFileLine(fileName: String): String =
    readFileLines(fileName).singleElementUnsafe

  def parseLineGroups[T](fileName: String, parser: List[String] => T): List[T] =
    readLineGroups(fileName) map parser

  def readLineGroups(fileName: String): List[List[String]] =
    readFileText(fileName)
      .split("\n\n")
      .map(_.split("\n").toList) // Some interesting line separator issues can appear, consider using "\\R"
      .toList

  def parseFileLines[T](fileName: String, parser: String => T): List[T] =
    readFileLines(fileName) map parser

  def readFileLines(fileName: String): List[String] =
    Using.resource(Source.fromResource(fileName))(_.getLines().toList)

  def readFileText(fileName: String): String =
    readFileLines(fileName).mkString("\n")
}
