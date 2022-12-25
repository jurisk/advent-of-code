package jurisk.adventofcode.y2022

import jurisk.adventofcode.y2022.Advent25.SnafuNumber.Mapping
import jurisk.collections.BiMap
import jurisk.collections.BiMap.BiDirectionalArrowAssociation
import jurisk.math.pow
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent25 {
  type Parsed = List[SnafuNumber]

  final case class SnafuNumber(numbers: List[Int]) {
    def asString: String = numbers.map(Mapping.leftToRightUnsafe).mkString

    def toDecimal: Long =
      numbers.reverse.zipWithIndex.map { case (n, idx) =>
        n * pow(5, idx)
      }.sum
  }

  object SnafuNumber {
    val Mapping: BiMap[Int, Char] = BiMap(
      -2 <-> '=',
      -1 <-> '-',
      0 <-> '0',
      1 <-> '1',
      2 <-> '2',
    )

    def fromDecimal(n: Long): SnafuNumber = {
      def toNonNormalizedDigits(n: Long): List[Int] = {
        val rem   = n / 5
        val digit = (n % 5).toInt
        digit :: (if (rem == 0) Nil else toNonNormalizedDigits(rem))
      }

      def normalize(numbers: List[Int]): List[Int] =
        numbers match {
          case Nil              => Nil
          case x :: xs if x < 3 => x :: normalize(xs)
          case x :: Nil         => (x - 5) :: 1 :: Nil
          case a :: b :: tail   => (a - 5) :: normalize((b + 1) :: tail)
        }

      SnafuNumber(normalize(toNonNormalizedDigits(n)).reverse)
    }

    def parse(s: String): SnafuNumber = {
      val numbers = s.toList.map(Mapping.rightToLeftUnsafe)
      val result  = SnafuNumber(numbers)
      result.asString shouldEqual s // sanity check
      result
    }
  }

  def parse(input: String): Parsed =
    input.parseList("\n", SnafuNumber.parse)

  def solve(data: Parsed): SnafuNumber =
    SnafuNumber.fromDecimal(data.map(_.toDecimal).sum)

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/25-test.txt")
    val realData = readFileText("2022/25.txt")

    SnafuNumber.parse("1=-0-2").toDecimal shouldEqual 1747

    SnafuNumber.fromDecimal(1) shouldEqual SnafuNumber.parse("1")
    SnafuNumber.fromDecimal(2) shouldEqual SnafuNumber.parse("2")
    SnafuNumber.fromDecimal(3) shouldEqual SnafuNumber.parse("1=")

    val test = parse(testData)
    val real = parse(realData)

    val testResult = solve(test)
    testResult.toDecimal shouldEqual 4890
    testResult.asString shouldEqual "2=-1=0"

    val realResult = solve(real)
    realResult.toDecimal shouldEqual 35951702021395L
    realResult.asString shouldEqual "2-21=02=1-121-2-11-0"
  }
}
