package jurisk.utils

import cats.effect.{IO, Resource}
import scala.io.BufferedSource
import scala.io.Source

object FileInputIO {
  private def sourceResource(fileName: String): Resource[IO, BufferedSource] = {
    val acquire                             = IO(Source.fromResource(fileName))
    val release: BufferedSource => IO[Unit] = source => IO(source.close())
    Resource.make(acquire)(release)
  }

  def readFileLines(fileName: String): IO[List[String]] =
    sourceResource(fileName).use { source =>
      IO(source.getLines().toList)
    }
}