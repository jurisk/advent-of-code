package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent15.Op.{Equals, Subtract}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable

object Advent15 {
//  type Input = List[HashValue]
//
//  object HashValue {
//    def parse(s: String): HashValue = {
//      s match {
//        case s"$s=$d" => HashValue(s, d.toInt)
//        case _ => s.failedToParse
//      }
//    }
//  }

  type Input = List[String]

  def calculateHash(s: String): Int = {
    var current = 0
    s.toList foreach { ch =>
      current += ch.toInt
      current *= 17
      current %= 256
    }
    current
  }
//
//  def becomes(hv: HashValue): Int = {
//
//  }

  def parse(input: String): Input =
    input.split(",").toList

  def part1(data: Input): Int =
    data.map(calculateHash).sum

  sealed trait Op {
    def orig: String
    def hash: Int = calculateHash(orig)
  }

  object Op {
    final case class Subtract(orig: String, s: String) extends Op
    final case class Equals(orig: String, s: String, value: Int) extends Op
  }

  def parseOp(s: String): Op = {
    s match {
      case s"$something-" => Subtract(s, something)
      case s"$something=$value" => Equals(s, something, value.toInt)
      case _ => s.failedToParse
    }
  }

  final case class LensValue(lens: String, focal: Int)

  final case class LensBox(lenses: Vector[LensValue]) {
    def subtract(s: String): LensBox = {
      val idx = lenses.indexWhere(_.lens == s)
      if (idx == -1) {
        this
      } else {
        val newContents = lenses.zipWithIndex.filter(_._2 != idx).map(_._1)
        LensBox(newContents)
      }
    }

    def replace(s: String, value: Int) = {
      val idx = lenses.indexWhere(_.lens == s)
      if (idx == -1) {
        LensBox(lenses :+ LensValue(s, value))
      } else {
        LensBox(
          lenses.updated(idx, LensValue(s, value))
        )
      }
    }

    def value(idx: Int): Int = {
      // One plus the box number of the lens in question.
      val a = (idx + 1)
      lenses.zipWithIndex.map { case (lens, index) =>
        a * (index + 1) * lens.focal
      }.sum
    }
  }

  object LensBox {
    def empty: LensBox = LensBox(Vector.empty)
  }

  def part2(data: Input): Int = {
    val ops = data.map(parseOp)
    var boxen: Vector[LensBox] = Vector.fill(256)(LensBox.empty)

    ops foreach { op =>
//      println(op)
//      println(boxen)
      op match {
        case Subtract(_, s) =>
          val idx = calculateHash(s)
          val old = boxen(idx)
          boxen = boxen.updated(idx, old.subtract(s))

        case Equals(_, s, value) =>
          val idx = calculateHash(s)
          val old = boxen(idx)
          boxen = boxen.updated(idx, old.replace(s, value))
      }
    }

    println(boxen)

    boxen.zipWithIndex.map { case (x, idx) => x.value(idx) } .sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/15.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
