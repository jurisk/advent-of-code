package jurisk.adventofcode.y2020

import cats.implicits.*
import jurisk.adventofcode.y2020.Advent11.*

import scala.util.Properties

object Advent11Spec extends App:
  val inputA = """
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
  
  def parse(x: String) = parseTestCases(x.split(Properties.lineSeparator).filter(_.nonEmpty).toList)
  
  val testsA = parse(inputA).fold((e) => sys.error(s"$e"), identity)

  val solved1 = solution1(testsA)
  val expected1 = 37
  require(solved1 == expected1, s"$solved1 did not equal $expected1")

  val solved2 = solution2(testsA)
  val expected2 = 26
  require(solved2 == expected2, s"$solved2 did not equal $expected2")
  
  println("Passed")
