package jurisk.adventofcode.y2020

import cats.implicits.*
import jurisk.adventofcode.y2020.Advent08
import jurisk.adventofcode.y2020.Advent08.{Execution, ExecutionResult}

import scala.util.Properties

object Advent08Spec extends App:
  val tests1 = """
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
  
  def parse(x: String) = Advent08.parseTestCases(x.split(Properties.lineSeparator).filter(_.nonEmpty).toList)
  
  val testCases1 = parse(tests1).getOrElse(sys.error("failed"))
  val solved1 = Advent08.solution1(testCases1)
  require(solved1 == 5)

  val test2 =
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

  val testCases2 = parse(test2).getOrElse(sys.error("Failed"))
  val program2 = Advent08.toProgram(testCases2)

  require(Execution(program2).run() == ExecutionResult.FinishedSuccessfully(8))

  require(Advent08.solution2(testCases1) == 8)
  
  println("Passed")
