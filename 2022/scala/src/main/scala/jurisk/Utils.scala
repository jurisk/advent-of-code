package jurisk

import scala.io.Source

object Utils {
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
