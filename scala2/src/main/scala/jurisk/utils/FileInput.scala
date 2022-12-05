package jurisk.utils

import jurisk.utils.Utils.IterableOps

import scala.io.Source
import scala.util.Using

object FileInput {
  def parseSingleFileLine[T](fileName: String, parser: Char => T): List[T] =
    readSingleFileLine(fileName).map(parser).toList

  def readSingleFileLine(fileName: String): String =
    readFileLines(fileName).map(_.trim).singleElementUnsafe

  def parseLineGroups[T](fileName: String, parser: List[String] => T): List[T] =
    readLineGroups(fileName) map parser

  def readLineGroups(fileName: String): List[List[String]] =
    readFileLines(fileName)
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toList)
      .toList

  def parseFileLines[T](fileName: String, parser: String => T): List[T] =
    readFileLines(fileName) map parser

  def readFileLines(fileName: String): List[String] =
    Using.resource(Source.fromResource(fileName))(_.getLines().toList)
}
