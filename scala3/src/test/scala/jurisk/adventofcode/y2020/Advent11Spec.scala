package jurisk.adventofcode.y2020

import jurisk.adventofcode.y2020.Advent11._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Properties

class Advent11Spec extends AnyFreeSpec:
  private val inputA = """
                         |L.LL.LL.LL
                         |LLLLLLL.LL
                         |L.L.L..L..
                         |LLLL.LL.LL
                         |L.LL.LL.LL
                         |L.LLLLL.LL
                         |..L.L.....
                         |LLLLLLLLLL
                         |L.LLLLLL.L
                         |L.LLLLL.LL
                         |""".stripMargin

  private def parse(x: String) = parseInput(
    x.split(Properties.lineSeparator).filter(_.nonEmpty).iterator
  )

  private val testsA = parse(inputA).fold(e => sys.error(s"$e"), identity)

  "solution1" in {
    val solved1   = solution1(testsA)
    val expected1 = 37
    solved1 shouldEqual expected1
  }

  "solution2" in {
    val solved2   = solution2(testsA)
    val expected2 = 26
    solved2 shouldEqual expected2
  }
