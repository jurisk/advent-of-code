package jurisk.adventofcode.y2020

import jurisk.adventofcode.y2020.Advent17.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*
import scala.util.Properties

class Advent17Spec extends AnyFreeSpec:
  private val inputA = """
    |.#.
    |..#
    |###
    |""".stripMargin
  
  private def parse(x: String) = parseTestCases(x.split(Properties.lineSeparator).filter(_.nonEmpty).toList)

  private val testsA = parse(inputA).fold(e => sys.error(s"$e"), identity)

  "solution1" in {
    val solved1 = solution1(testsA)
    val expected1 = 112
    solved1 shouldEqual expected1
  }

  "solution2" in {
    val solved2 = solution2(testsA)
    val expected2 = 848
    solved2 shouldEqual expected2
  }
