package jurisk

import scala.io.Source

object Utils {
  def readFileLines(fileName: String): List[String] = {
    val source = Source.fromResource(fileName)
    try source.getLines.toList
    finally source.close()
  }
}
