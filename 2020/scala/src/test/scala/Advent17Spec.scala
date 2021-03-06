import Advent17._
import cats.implicits._

object Advent17Spec extends App:
  val inputA = """
    |.#.
    |..#
    |###
    |""".stripMargin
  
  def parse(x: String) = parseTestCases(x.split("\n").filter(_.nonEmpty).toList)
  
  val testsA = parse(inputA).fold((e) => sys.error(s"$e"), identity)

  val solved1 = solution1(testsA)
  val expected1 = 112
  require(solved1 == expected1, s"$solved1 did not equal $expected1")

  val solved2 = solution2(testsA)
  val expected2 = 848
  require(solved2 == expected2, s"$solved2 did not equal $expected2")
  
  println("Passed")
