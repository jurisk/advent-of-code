package jurisk.adventofcode.y2020

import jurisk.adventofcode.y2020.Advent08.Execution
import jurisk.adventofcode.y2020.Advent08.ExecutionResult
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Properties

class Advent08Spec extends AnyFreeSpec:
  private val tests1 = """
                         |nop +0
                         |acc +1
                         |jmp +4
                         |acc +3
                         |jmp -3
                         |acc -99
                         |acc +1
                         |jmp -4
                         |acc +6
                         |""".stripMargin

  private def parse(x: String) = Advent08.parseInput(
    x.split(Properties.lineSeparator).filter(_.nonEmpty).iterator
  )

  private val testCases1 = parse(tests1).getOrElse(sys.error("failed"))

  "solution1" in {
    val solved1 = Advent08.solution1(testCases1)
    solved1 shouldEqual 5
  }

  private val test2 =
    """
      |nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |nop -4
      |acc +6
      |""".stripMargin

  private val testCases2 = parse(test2).getOrElse(sys.error("Failed"))

  "solution2" in {
    val program2 = Advent08.toProgram(testCases2)

    Execution(program2).run() shouldEqual ExecutionResult.FinishedSuccessfully(
      8
    )

    Advent08.solution2(testCases1) shouldEqual 8
  }
