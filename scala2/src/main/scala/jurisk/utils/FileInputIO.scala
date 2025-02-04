package jurisk.utils

import cats.effect.IO
import cats.effect.Resource

import java.io.File
import java.io.FileWriter
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

  def readFileText(fileName: String): IO[String] = for {
    lines <- readFileLines(fileName)
  } yield lines.mkString("\n")

  def writeFileText(fileName: String, text: String): IO[Unit] =
    Resource
      .make(IO(new FileWriter(new File(fileName)))) { writer =>
        IO(writer.close())
      }
      .use { writer =>
        IO(writer.write(text))
      }
}
