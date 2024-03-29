package jurisk.adventofcode.y2023

import jurisk.utils.CollectionOps.VectorOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

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
    def label: Label
    def originalString: String
    def applyTo(lenses: List[Lens]): List[Lens]
  }

  object Step {
    final private case class Remove(label: String) extends Step {
      def originalString: Label = s"$label-"

      def applyTo(lenses: List[Lens]): List[Lens] = {
        @tailrec
        def helper(remainingLenses: List[Lens], acc: List[Lens]): List[Lens] =
          remainingLenses match {
            case head :: tail if head.label == label =>
              acc.reverse ::: tail
            case head :: tail                        =>
              helper(tail, head :: acc)
            case Nil                                 =>
              acc.reverse
          }

        helper(lenses, List.empty)
      }
    }

    final private case class Replace(
      label: Label,
      focalLength: FocalLength,
    ) extends Step {
      def originalString: Label = s"$label=$focalLength"

      def applyTo(lenses: List[Lens]): List[Lens] = {
        @tailrec
        def helper(remainingLenses: List[Lens], acc: List[Lens]): List[Lens] =
          remainingLenses match {
            case head :: tail if head.label == label =>
              acc.reverse ::: Lens(label, focalLength) :: tail
            case head :: tail                        =>
              helper(tail, head :: acc)
            case Nil                                 =>
              acc.reverse ::: Lens(label, focalLength) :: Nil
          }

        helper(lenses, List.empty)
      }
    }

    def parse(s: String): Step =
      s match {
        case s"$label-"             => Remove(label)
        case s"$label=$focalLength" => Replace(label, focalLength.toInt)
        case _                      => s.failedToParse
      }
  }

  final case class Lens(label: String, focalLength: Int)

  final case class LensBox(lenses: List[Lens]) {
    def value: Int =
      lenses.zipWithIndex.map { case (lens, index) =>
        (index + 1) * lens.focalLength
      }.sum
  }

  private object LensBox {
    def empty: LensBox = LensBox(List.empty)
  }

  def part1(data: Steps): Int =
    data.map(op => calculateHash(op.originalString)).sum

  def part2(ops: Steps): Int = {
    val LensCount = 256
    val boxen     = ops.foldLeft(Vector.fill(LensCount)(LensBox.empty)) {
      case (acc, op) =>
        val idx = calculateHash(op.label)
        acc.updatedWith(idx)(lensBox => LensBox(op.applyTo(lensBox.lenses)))
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
