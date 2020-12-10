import cats.implicits._

object Advent10Spec extends App:
  val inputA = """
    |16
    |10
    |15
    |5
    |1
    |11
    |7
    |19
    |6
    |12
    |4
    |""".stripMargin
  
  def parse(x: String) = Advent10.parseTestCases(x.split("\n").filter(_.nonEmpty).toList)
  
  val testsA = parse(inputA).getOrElse(sys.error("failed"))

  val solved1 = Advent10.solution1(testsA)
  
  val expected1 = 7 * 5
  require(solved1 == expected1, s"$solved1 did not equal $expected1")

  val inputB =
    """
      |28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  val testsB = parse(inputB).getOrElse(sys.error("failed"))
  val solved2 = Advent10.solution1(testsB)
  
  val expected2 = 22 * 10
  require(solved2 == expected2, s"$solved2 did not equal $expected2")

  assert(Advent10.solution2(1 :: Nil) == 1)
  assert(Advent10.solution2(1 :: 2 :: Nil) == 2)
  assert(Advent10.solution2(1 :: 2 :: 3 :: Nil) == 4)

  val solved3 = Advent10.solution2(testsA)
  val expected3 = 8
  require(solved3 == expected3, s"$solved3 did not equal $expected3")
  
  val solved4 = Advent10.solution2(testsB)
  val expected4 = 19208
  require(solved4 == expected4, s"$solved4 did not equal $expected4")

  println("Passed")
