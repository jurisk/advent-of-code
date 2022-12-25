package jurisk.adventofcode.y2022

import jurisk.math.pow
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent25 {
  type Parsed = List[SnafuNumber]

  final case class SnafuNumber(numbers: List[Int]) {
    def asString: String = numbers.map {
      case -2 => '='
      case -1 => '-'
      case 0  => '0'
      case 1  => '1'
      case 2  => '2'
      case n  => s"$n".fail
    }.mkString

    def toDecimal: Long =
      numbers.reverse.zipWithIndex.map { case (n, idx) =>
        n * pow(5, idx)
      }.sum
  }

  object SnafuNumber {
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
          case x :: xs          =>
            (x - 5) ::
              xs match {
                case h :: t => normalize((h + 1) :: t)
                case Nil    => 1 :: Nil
              }
        }

      SnafuNumber(normalize(toNonNormalizedDigits(n)).reverse)
    }

    def parse(s: String): SnafuNumber = {
      val numbers = s.toList.map {
        case '=' => -2
        case '-' => -1
        case '0' => 0
        case '1' => 1
        case '2' => 2
        case ch  => s"$ch".fail
      }

      val result = SnafuNumber(numbers)
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
