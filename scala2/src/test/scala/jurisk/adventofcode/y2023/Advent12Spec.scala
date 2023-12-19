package jurisk.adventofcode.y2023

import cats.effect.testing.scalatest.AsyncIOSpec
import jurisk.adventofcode.y2023.Advent12._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers._

class Advent12Spec extends AsyncFreeSpec with AsyncIOSpec {
  def testPart1(input: String, expected: Long): Unit =
    input in {
      val row = Row.parse(input)
      row.arrangements shouldEqual expected
      row.bruteForceArrangements shouldEqual expected
    }

  def testPart2(input: String, expected: Long): Unit =
    input in {
      Row.parse(input).expand(5).arrangements shouldEqual expected
    }

  "part 1 arrangements 1" - {
    testPart1("# 1", 1)
    testPart1("? 1", 1)
    testPart1(". 1", 0)
    testPart1("# 2", 0)
    testPart1("?? 1", 2)
    testPart1("??? 1,1", 1)
    testPart1("???.### 1,1,3", 1)
    testPart1(".??..??...?##. 1,1,3", 4)
  }

  "part 1 arrangements 2" - {
    testPart1("?#?#?#?#?#?#?#? 1,3,1,6", 1)
    testPart1("????.#...#... 4,1,1", 1)
    testPart1("????.######..#####. 1,6,5", 4)
    testPart1("??????? 2,1", 10)
    testPart1("?###? 3", 1)
    testPart1("###? 3", 1)
    testPart1("?###???????? 3,2,1", 10)
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
      for {
        input    <- parseFile("2023/12.txt")
        solution <- part1(input)
      } yield solution shouldEqual 7694
    }
  }

  "part 2" - {
    "real" in {
      for {
        input    <- parseFile("2023/12.txt")
        solution <- part2(input)
      } yield solution shouldEqual 5071883216318L
    }
  }
}
