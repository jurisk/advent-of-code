import AdventApp.ErrorMessage

object Advent01 extends SingleLineAdventApp[Int, Long]:
  def fileName: String = "01.txt"

  def solve(testCases: List[Int], n: Int): Long =
    testCases
      .combinations(n)
      .find { _.sum == 2020 }
      .getOrElse(sys.error("Failed"))
      .product

  def solution1(testCases: List[Int]): Long = solve(testCases, 2)
  def solution2(testCases: List[Int]): Long = solve(testCases, 3)
  
  def parseLine(line: String): Either[ErrorMessage, Int] = 
    line.toIntOption.toRight(ErrorMessage("Failed to parse"))
