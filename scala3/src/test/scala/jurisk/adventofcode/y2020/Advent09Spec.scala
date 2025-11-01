package jurisk.adventofcode.y2020

import jurisk.adventofcode.y2020.Advent09.solution2
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Properties

class Advent09Spec extends AnyFreeSpec:
  private val tests1 = """
                         |20
                         |15
                         |25
                         |47
                         |40
                         |62
                         |55
                         |65
                         |95
                         |102
                         |117
                         |150
                         |182
                         |127
                         |219
                         |299
                         |277
                         |309
                         |576""".stripMargin

  private def parse(x: String) = Advent09.parseInput(
    x.split(Properties.lineSeparator).filter(_.nonEmpty).iterator
  )

  private val testCases1 = parse(tests1).getOrElse(sys.error("failed"))

  "solution1" in {
    Advent09.solution1(testCases1, 5) shouldEqual Some(127)
  }

  "solution2" in {
    solution2(testCases1, 5) shouldEqual Some(62)
  }
