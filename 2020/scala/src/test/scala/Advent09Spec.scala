import cats.implicits._

object Advent09Spec extends App:
  val tests1 = """
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
  
  def parse(x: String) = Advent09.parseTestCases(x.split("\n").filter(_.nonEmpty).toList)
  
  val testCases1 = parse(tests1).getOrElse(sys.error("failed"))

  val solved1 = Advent09.solution1(testCases1, 5)
  
  val expected1 = Some(127)
  require(solved1 == expected1, s"$solved1 did not equal $expected1")

  val solved2 = Advent09.solution2(testCases1, 5)
  
  val expected2 = Some(62)
  require(solved2 == expected2, s"$solved2 did not equal $expected2")

  println("Passed")
