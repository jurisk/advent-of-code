package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent12._
import org.scalatest.matchers.should.Matchers._

class Advent12Spec extends AnyFreeSpec {
  def testPart1(input: String, expected: Long): Unit =
    input in {
      Row.parse(input).arrangements shouldEqual expected
    }

  def testPart2(input: String, expected: Long): Unit =
    input in {
      Row.parse(input).expand(5).arrangements shouldEqual expected
    }

  "part 1 arrangements" - {
    testPart1("# 1", 1)
    testPart1("? 1", 1)
    testPart1(". 1", 0)
    testPart1("# 2", 0)
    testPart1("?? 1", 2)
    testPart1("??? 1,1", 1)
    testPart1("???.### 1,1,3", 1)
    testPart1(".??..??...?##. 1,1,3", 4)
    Row.parse("?#?#?#?#?#?#?#? 1,3,1,6").arrangements shouldEqual 1
    Row.parse("????.#...#... 4,1,1").arrangements shouldEqual 1
    Row.parse("????.######..#####. 1,6,5").arrangements shouldEqual 4
    Row.parse("??????? 2,1").arrangements shouldEqual 10
    Row.parse("?###? 3").arrangements shouldEqual 1
    Row.parse("###? 3").arrangements shouldEqual 1
    Row.parse("?###???????? 3,2,1").arrangements shouldEqual 10
  }

  "part 2 arrangements" - {
    testPart2("???.### 1,1,3", 1)
    testPart2("????.######..#####. 1,6,5", 2500)
    testPart2(".??..??...?##. 1,1,3", 16384)
    testPart2("?###???????? 3,2,1", 506250)
    testPart2(".#????????????????? 1,4,3,2,2", 120681045)
  }

  "part 1" - {
    "real" in {
      part1(parseFile("2023/12.txt")) shouldEqual 7694
    }
  }

  "part 2" - {
    "real" in {
      part2(parseFile("2023/12.txt")) shouldEqual 5071883216318L
    }
  }
}
