package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent12._
import org.scalatest.matchers.should.Matchers._

class Advent12Spec extends AnyFreeSpec {
  "arrangements" - {
    "a" in {
      Row.parse("# 1").arrangements shouldEqual 1
      Row.parse("? 1").arrangements shouldEqual 1
      Row.parse(". 1").arrangements shouldEqual 0
    }

    "aa" in {
      Row.parse("# 2").arrangements shouldEqual 0
    }

    "b" in {
      Row.parse("?? 1").arrangements shouldEqual 2
    }

    "c" in {
      Row.parse("??? 1,1").arrangements shouldEqual 1
    }

    "1" in {
      Row.parse("???.### 1,1,3").arrangements shouldEqual 1
    }

    "1 expand" in {
      Row.parse("???.### 1,1,3").expandedArrangements(5) shouldEqual 1
    }

    "2" in {
      Row.parse(".??..??...?##. 1,1,3").arrangements shouldEqual 4
    }

    "3" in {
      Row.parse("?#?#?#?#?#?#?#? 1,3,1,6").arrangements shouldEqual 1
    }

    "4" in {
      Row.parse("????.#...#... 4,1,1").arrangements shouldEqual 1
    }

    "5" in {
      Row.parse("????.######..#####. 1,6,5").arrangements shouldEqual 4
    }

    "6a" in {
      Row.parse("??????? 2,1").arrangements shouldEqual 10
    }

    "6b" in {
      Row.parse("?###? 3").arrangements shouldEqual 1
    }

    "6c" in {
      Row.parse("###? 3").arrangements shouldEqual 1
    }

    "6" in {
      Row.parse("?###???????? 3,2,1").arrangements shouldEqual 10
    }

    "6 expand" in {
      Row.parse("?###???????? 3,2,1").expandedArrangements(5) shouldEqual 506250
    }

    "2500" in {
      Row
        .parse("????.######..#####. 1,6,5")
        .expandedArrangements(5) shouldEqual 2500
    }

    "slow" in {
      Row
        .parse(
          ".#??????????????????.#??????????????????.#??????????????????.#??????????????????.#????????????????? 1,4,3,2,2,1,4,3,2,2,1,4,3,2,2,1,4,3,2,2,1,4,3,2,2"
        )
        .arrangements shouldEqual 120681045
    }
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
