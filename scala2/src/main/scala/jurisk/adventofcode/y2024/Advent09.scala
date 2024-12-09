package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import jurisk.utils.CollectionOps.SeqOps
import jurisk.utils.CollectionOps.VectorOps
import jurisk.utils.FileInput._

import scala.annotation.tailrec

object Advent09 {
  sealed trait Node extends Product with Serializable
  object Node {
    final case class File(fileId: FileId, blocks: Int) extends Node
    final case class Free(blocks: Int)                 extends Node
  }

  private type FileId = Int
  type Input          = Vector[Node]
  type N              = Long

  def parse(input: String): Input =
    input.toList
      .map(_.toString.toInt)
      .zipWithIndex
      .map { case (length, index) =>
        if (index % 2 == 0) {
          Node.File(index / 2, length)
        } else {
          Node.Free(length)
        }
      }
      .toVector

  private def decompress(nodes: Vector[Node]): Vector[Option[FileId]] =
    nodes.flatMap {
      case Node.File(fileId, blocks) => Vector.fill(blocks)(Some(fileId))
      case Node.Free(blocks)         => Vector.fill(blocks)(None)
    }

  private def score(decompressed: Vector[Option[FileId]]): N =
    decompressed.zipWithIndex.map { case (value, index) =>
      value match {
        case Some(fileId) => fileId.toLong * index
        case None         => 0
      }
    }.sum

  def part1(data: Input): N = {
    def compactBlocks(list: Vector[Option[FileId]]): Vector[Option[FileId]] = {
      @tailrec
      def f(
        files: Vector[Option[FileId]],
        head: Int,
        tail: Int,
      ): Vector[Option[FileId]] =
        if (head >= tail) {
          files
        } else {
          (files(head), files(tail)) match {
            case (None, None)    =>
              f(files, head, tail - 1)
            case (None, Some(t)) =>
              f(
                files.updated(head, t.some).updated(tail, None),
                head + 1,
                tail - 1,
              )
            case _               =>
              f(files, head + 1, tail)
          }
        }

      f(list, 0, list.length - 1)
    }

    score(compactBlocks(decompress(data)))
  }

  def part2(nodes: Input): N = {
    val toMove: Vector[Node.File] = nodes.reverse flatMap { node =>
      node match {
        case Node.Free(_)        => none[Node.File]
        case x @ Node.File(_, _) =>
          x.some
      }
    }

    val compressed = toMove.foldLeft(nodes) { case (acc, file) =>
      val idxFile = acc.firstIndexOf(file)
      val idxFree = acc.firstIndexWhere {
        case Node.Free(blocks) =>
          blocks >= file.blocks
        case _                 => false
      }

      (idxFile, idxFree) match {
        case (Some(idxFile), Some(idxFree)) if idxFile > idxFree =>
          acc(idxFree) match {
            case Node.Free(freeBlocks) =>
              val extras = freeBlocks - file.blocks

              require(extras >= 0)

              if (extras == 0) {
                acc
                  .updated(idxFile, Node.Free(file.blocks))
                  .updated(idxFree, file)
              } else {
                acc
                  .updated(idxFile, Node.Free(file.blocks))
                  .replaceAt(idxFree, Vector(file, Node.Free(extras)))
              }
            case _                     =>
              acc
          }
        case _                                                   =>
          acc
      }
    }

    score(decompress(compressed))
  }

  private def printNodes(v: Vector[Node]): Unit = {
    val s = v flatMap { n =>
      n match {
        case Node.File(fileId, blocks) =>
          List.fill(blocks)(fileId.toString).mkString
        case Node.Free(blocks)         => List.fill(blocks)(".").mkString
      }
    }
    println(s.mkString)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/09$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
