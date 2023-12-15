package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent15.Op.{Equals, Subtract}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent15 {
  type Input = List[Op]

  def calculateHash(s: String): Int =
    s.toList.foldLeft(0) { case (acc, ch) =>
      ((acc + ch.toInt) * 17) % 256
    }

  def parse(input: String): Input =
    input.split(",").map(Op.parse).toList

  def part1(data: Input): Int =
    data.map(op => calculateHash(op.originalString)).sum

  sealed trait Op {
    def originalString: String
  }

  object Op {
    final case class Subtract(originalString: String, s: String) extends Op
    final case class Equals(originalString: String, s: String, value: Int)
        extends Op

    def parse(s: String): Op =
      s match {
        case s"$something-"       => Subtract(s, something)
        case s"$something=$value" => Equals(s, something, value.toInt)
        case _                    => s.failedToParse
      }
  }

  final case class LensValue(lens: String, focal: Int)

  final case class LensBox(lenses: Vector[LensValue]) {
    def subtract(s: String): LensBox = {
      val idx = lenses.indexWhere(_.lens == s)
      if (idx == -1) {
        this
      } else {
        // TODO: improve
        val updated = lenses.zipWithIndex.filter(_._2 != idx).map(_._1)
        LensBox(updated)
      }
    }

    def replace(s: String, value: Int): LensBox = {
      val idx     = lenses.indexWhere(_.lens == s)
      val updated = if (idx == -1) {
        lenses :+ LensValue(s, value)
      } else {
        lenses.updated(idx, LensValue(s, value))
      }
      LensBox(updated)
    }

    def value(idx: Int): Int =
      lenses.zipWithIndex.map { case (lens, index) =>
        (idx + 1) * (index + 1) * lens.focal
      }.sum
  }

  object LensBox {
    def empty: LensBox = LensBox(Vector.empty)
  }

  def part2(ops: Input): Int = {
    val boxen = ops.foldLeft(Vector.fill(256)(LensBox.empty)) {
      case (acc, op) =>
        op match {
          case Subtract(_, s) =>
            val idx = calculateHash(s)
            val old = acc(idx)
            acc.updated(idx, old.subtract(s))

          case Equals(_, s, value) =>
            val idx = calculateHash(s)
            val old = acc(idx)
            acc.updated(idx, old.replace(s, value))
        }
    }

    boxen.zipWithIndex.map { case (x, idx) => x.value(idx) }.sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/15.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
