package jurisk.utils

import jurisk.utils.Utils.IterableOps

import scala.io.Source

object FileInput {
  def parseFirstFileLine[T](fileName: String, parser: Char => T): List[T] =
    readFirstFileLine(fileName).map(parser).toList

  def readFirstFileLine(fileName: String): String =
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

  def readFileLines(fileName: String): List[String] = {
    val source = Source.fromResource(fileName)
    try source.getLines.toList
    finally source.close()
  }
}
