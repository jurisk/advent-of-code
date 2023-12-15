package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent15.Step.{Remove, Replace}
import jurisk.utils.CollectionOps.{SeqOps, VectorOps}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent15 {
  private type Steps       = List[Step]
  private type Label       = String
  private type FocalLength = Int

  def calculateHash(s: String): Int =
    s.toList.foldLeft(0) { case (acc, ch) =>
      ((acc + ch.toInt) * 17) % 256
    }

  def parse(input: String): Steps =
    input.split(",").map(Step.parse).toList

  sealed trait Step {
    def originalString: String
  }

  object Step {
    final case class Remove(label: String) extends Step {
      override def originalString: Label = s"$label-"
    }

    final case class Replace(
      label: Label,
      focalLength: FocalLength,
    ) extends Step {
      override def originalString: Label = s"$label=$focalLength"
    }

    def parse(s: String): Step =
      s match {
        case s"$label-"             => Remove(label)
        case s"$label=$focalLength" => Replace(label, focalLength.toInt)
        case _                      => s.failedToParse
      }
  }

  final case class Lens(label: String, focalLength: Int)

  final case class LensBox(lenses: Vector[Lens]) {
    def subtract(label: Label): LensBox =
      lenses.firstIndexWhere(_.label == label) match {
        case Some(idx) => LensBox(lenses.removeAt(idx))
        case None      => this
      }

    def replace(label: Label, value: FocalLength): LensBox =
      LensBox(
        lenses.firstIndexWhere(_.label == label) match {
          case Some(idx) => lenses.updated(idx, Lens(label, value))
          case None      => lenses :+ Lens(label, value)
        }
      )

    def value: Int =
      lenses.zipWithIndex.map { case (lens, index) =>
        (index + 1) * lens.focalLength
      }.sum
  }

  private object LensBox {
    def empty: LensBox = LensBox(Vector.empty)
  }

  def part1(data: Steps): Int =
    data.map(op => calculateHash(op.originalString)).sum

  def part2(ops: Steps): Int = {
    val LensCount = 256
    val boxen     = ops.foldLeft(Vector.fill(LensCount)(LensBox.empty)) {
      case (acc, op) =>
        op match {
          case Remove(label) =>
            val idx = calculateHash(label)
            acc.updatedWith(idx)(_.subtract(label))

          case Replace(label, focalLength) =>
            val idx = calculateHash(label)
            acc.updatedWith(idx)(_.replace(label, focalLength))
        }
    }

    boxen.zipWithIndex.map { case (contents, idx) =>
      (idx + 1) * contents.value
    }.sum
  }

  def parseFile(fileName: String): Steps =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Steps = parseFile("2023/15.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
