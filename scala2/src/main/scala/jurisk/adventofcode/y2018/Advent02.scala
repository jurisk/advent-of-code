package jurisk.adventofcode.y2018

import jurisk.utils.FileInput.readFileLines
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent02 {
  private def readFileAndParse(fileName: String): List[String] =
    readFileLines(fileName)

  private def hasNOfSomeLetter(s: String, n: Int): Boolean =
    s.toList.counts.values.toSet.contains(n)

  def part1(data: List[String]): Int = {
    val a = data.count(s => hasNOfSomeLetter(s, 2))
    val b = data.count(s => hasNOfSomeLetter(s, 3))
    a * b
  }

  private def differByOneChar(a: String, b: String): Option[String] = {
    require(a.length == b.length)

    if (a == b) None
    else {
      val common = (a zip b) flatMap {
        case (ac, bc) if ac == bc => Some(ac)
        case _                    => None
      }

      if (common.length == a.length - 1) Some(common.mkString) else None
    }
  }

  def part2(data: List[String]): String = {
    // O(N^2), but it should be fine
    val result = data flatMap { a =>
      data flatMap { b =>
        differByOneChar(a, b)
      }
    }

    result.head
  }

  def main(args: Array[String]): Unit = {
    val real = readFileAndParse("2018/02.txt")

    part1(real) shouldEqual 5952
    part2(real) shouldEqual "krdmtuqjgwfoevnaboxglzjph"
  }
}
