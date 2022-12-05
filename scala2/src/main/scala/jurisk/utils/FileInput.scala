package jurisk.utils

import jurisk.utils.Utils.IterableOps

import scala.io.Source

object FileInput {
  def readFirstFileLine(fileName: String): String = {
    readFileLines(fileName).map(_.trim).singleElementUnsafe
  }

  def readLineGroups(fileName: String): List[List[String]] =
    readFileLines(fileName)
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toList)
      .toList

  def readFileLines(fileName: String): List[String] = {
    val source = Source.fromResource(fileName)
    try source.getLines.toList
    finally source.close()
  }
}
