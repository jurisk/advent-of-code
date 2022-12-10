package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.utils.FileInput.parseSingleFileLine
import org.scalatest.matchers.should.Matchers._

object Advent05 {
  type Polymer = Vector[Elem]
  type Result  = Int

  final case class Elem(value: Char) extends AnyVal {
    def canReduce(other: Elem): Boolean =
      value.toLower == other.value.toLower && value != other.value

    override def toString: String = s"$value"
  }

  object Elem {
    val AllPairs: List[(Elem, Elem)] =
      ('a' to 'z').toList map { ch =>
        (Elem(ch), Elem(ch.toUpper))
      }

    def parse(x: Char): Elem =
      if (x.isLetter) Elem(x)
      else sys.error(s"Failed to recognize: $x")
  }

  def readFileAndParse(fileName: String): Polymer =
    parseSingleFileLine(fileName, Elem.parse).toVector

  private def reducePolymer(polymer: Polymer): Polymer =
    polymer.foldLeft(Vector.empty[Elem]) { case (acc, elem) =>
      if (acc.lastOption.exists(elem.canReduce)) acc.init
      else acc.appended(elem)
    }

  def part1(polymer: Polymer): Result = reducePolymer(polymer).size

  def part2(data: Polymer): Result = {
    val options = Elem.AllPairs.map { case (a, b) =>
      val filtered = data.filterNot { x =>
        x == a || x == b
      }

      print(s"Checking with '$a and $b' removed... ")
      val result = part1(filtered)
      println(s"$result")
      result
    }

    options.min
  }

  def main(args: Array[String]): Unit = {
    val test = readFileAndParse("2018/05-test.txt")
    val real = readFileAndParse("2018/05.txt")

    part1(test) shouldEqual 10
    part1(real) shouldEqual 11118

    part2(test) shouldEqual 4
    part2(real) shouldEqual 6948
  }
}
