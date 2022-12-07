package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent07 {
  type Parsed    = List[Entry]
  type Entry     = (Command, List[OutputLine])
  type Processed = Directory
  type Result1   = Long
  type Result2   = Long

  sealed trait OutputLine
  object OutputLine {
    case class Dir(name: String)              extends OutputLine
    case class File(name: String, size: Long) extends OutputLine

    def parse(s: String): OutputLine = {
      val (a, b) = s.splitPairUnsafe(" ")
      a match {
        case "dir" => Dir(b)
        case _     => File(b, a.toLong)
      }
    }
  }

  final case class Directory(
    directories: Map[String, Directory],
    files: Map[String, Long],
  ) {
    def withSomeDirDeleted: List[Directory] =
      directories.keys.toList flatMap { key =>
        copy(directories = directories - key) :: directories(
          key
        ).withSomeDirDeleted.map(d =>
          copy(directories = directories + (key -> d))
        )
      }

    def totalSize: Long                              =
      directories.values.map(_.totalSize).sum + files.values.sum
    def allDirectoriesIncludingSelf: List[Directory] =
      this :: directories.values.toList.flatMap(_.allDirectoriesIncludingSelf)

    def addFile(path: List[String], name: String, size: Long): Directory =
      path match {
        case Nil =>
          copy(files = files + (name -> size))

        case h :: t =>
          val existing =
            directories.getOrElse(h, Directory(Map.empty, Map.empty))

          val updated = existing.addFile(t, name, size)

          copy(
            directories = directories + (h -> updated)
          )
      }
  }

  sealed trait Command

  object Command {
    case object CdRoot            extends Command
    case class CdDir(dir: String) extends Command
    case object CdUp              extends Command
    case object Ls                extends Command

    def parse(s: String): Command =
      s match {
        case "$ ls"                     => Ls
        case x if x.startsWith("$ cd ") =>
          val dir = x.drop("$ cd ".length)
          dir match {
            case ".." => CdUp
            case "/"  => CdRoot
            case _    => CdDir(dir)
          }
        case _                          => sys.error(s)
      }
  }

  def process(parsed: Parsed): Processed = {
    var currentDir: List[String] = Nil
    var output: Directory        = Directory(Map.empty, Map.empty)

    parsed foreach { case (command, outputLines) =>
      command match {
        case Command.CdRoot =>
          currentDir = Nil

        case Command.CdDir(dir) =>
          currentDir = currentDir ::: List(dir)

        case Command.CdUp =>
          currentDir = currentDir.init

        case Command.Ls =>
          outputLines foreach {
            case OutputLine.Dir(_)           =>
            case OutputLine.File(name, size) =>
              output = output.addFile(currentDir, name, size)
          }
      }
    }

    output
  }

  def parse(fileName: String): Parsed = {
    val lines = readFileLines(fileName)

    @tailrec
    def f(data: List[String], acc: List[Entry]): List[Entry] =
      data match {
        case Nil    => acc
        case h :: t =>
          val command    = Command.parse(h)
          val output     = t.takeWhile(!_.startsWith("$ "))
          val thisOutput = (command, output.map(OutputLine.parse))
          val remains    = t.drop(output.size)
          f(remains, acc ::: List(thisOutput))
      }

    f(lines, Nil)
  }

  def part1(data: Parsed, atMost: Long): Result1 = {
    val processed = process(data)
    processed.allDirectoriesIncludingSelf
      .map(_.totalSize)
      .filter(_ <= atMost)
      .sum
  }

  def part2(data: Parsed, limit: Long): Result2 = {
    val processed = process(data)
    val options   = processed.withSomeDirDeleted
    val space     = options.map(_.totalSize).filter(_ <= limit).max
    processed.totalSize - space
  }

  def main(args: Array[String]): Unit = {
    val test = parse("2022/07-test.txt")
    val real = parse("2022/07.txt")

    part1(test, 100000) shouldEqual 95437
    part1(real, 100000) shouldEqual 1428881

    val MaxTaken = 70000000 - 30000000

    part2(test, MaxTaken) shouldEqual 24933642
    part2(real, MaxTaken) shouldEqual 10475598
  }
}
