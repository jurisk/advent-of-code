package jurisk.adventofcode.y2022

import jurisk.adventofcode.y2022.Advent13.Packet.Lst
import jurisk.adventofcode.y2022.Advent13.Packet.Nmb
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import mouse.all._
import org.scalatest.matchers.should.Matchers._

import scala.math.Ordering.Implicits._

object Advent13 {
  type Parsed = List[Pair]

  final case class Pair(
    left: Packet,
    right: Packet,
  ) {
    def packets: List[Packet] = left :: right :: Nil
    def rightOrder: Boolean   = left < right
  }

  sealed trait Packet {
    def compare(other: Packet): Int = {
      import scala.math.Ordered.orderingToOrdered

      (this, other) match {
        case (Nmb(a), Nmb(b)) => a compare b
        case (Lst(a), Lst(b)) => a compare b
        case (Nmb(a), Lst(b)) => Lst(Nmb(a)) compare Lst(b)
        case (Lst(a), Nmb(b)) => Lst(a) compare Lst(Nmb(b))
      }
    }
  }

  object Packet {
    implicit val ordering: Ordering[Packet] = (x: Packet, y: Packet) =>
      x compare y

    final case class Nmb(value: Int)               extends Packet {
      override def toString: String = value.toString
    }
    final case class Lst(containing: List[Packet]) extends Packet {
      override def toString: String =
        containing.map(_.toString).mkString("[", ",", "]")
    }

    object Lst {
      def apply(data: Packet*): Lst = new Lst(data.toList)
    }

    def parse(s: String): Packet = {
      import cats.parse.{Parser => P}
      import cats.parse.Rfc5234.digit

      val packetParser: P[Packet] = P.recursive[Packet] { recurse =>
        // Must be .recursive to avoid a StackOverFlowException
        val nmbParser: P[Packet] = digit.rep.string.map(_.toInt).map(Nmb)
        val lstParser: P[Lst]    =
          (P.char('[') *> recurse.repSep0(P.char(',')) <* P.char(']'))
            .map(Lst(_))
        nmbParser | lstParser
      }

      packetParser.parseAll(s) match {
        case Left(value)  => value.toString().fail
        case Right(value) => value
      }
    }
  }

  object Pair {
    def parse(s: String): Pair = {
      val List(a, b) = s.splitLines
      Pair(Packet.parse(a), Packet.parse(b))
    }
  }

  def parse(data: String): Parsed =
    data.parseSections(Pair.parse)

  def part1(data: Parsed): Int =
    data.zipWithIndex.flatMap { case (pair, index) =>
      println(s"== Pair ${index + 1} == ")
      println(s"- Compare ${pair.left} vs ${pair.right}")
      println(s"- Right order = ${pair.rightOrder}")
      println()
      pair.rightOrder.option(index + 1)
    }.sum

  def part2(data: Parsed): Int = {
    // NB - sorting is actually not needed, we can just count how many packets are < DividerX, but this is easier to
    // write and understand.

    val DividerA: Packet = Lst(Lst(Nmb(2)))
    val DividerB: Packet = Lst(Lst(Nmb(6)))

    val allPackets: List[Packet] =
      DividerA :: DividerB :: data.flatMap(_.packets)

    val sorted = allPackets.sorted

    val indexA = sorted.indexOf(DividerA)
    val indexB = sorted.indexOf(DividerB)

    (indexA + 1) * (indexB + 1)
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/13-test.txt")
    val realData = readFileText("2022/13.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 13
    part1(real) shouldEqual 5529

    part2(test) shouldEqual 140
    part2(real) shouldEqual 27690
  }
}
