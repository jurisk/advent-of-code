package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Advent10._

class Advent10Spec extends AnyFlatSpec {
  "Advent10" should "test part 1 1" in {
    part1(parseFile("2023/10-test-1.txt")) shouldEqual 4
  }

  it should "test part 1 2" in {
    part1(parseFile("2023/10-test-2.txt")) shouldEqual 8
  }

  it should "real part 1" in {
    part1(parseFile("2023/10.txt")) shouldEqual 6768
  }

  it should "test part 2 1" in {
    part2(parseFile("2023/10-test-3.txt")) shouldEqual 4
  }

  it should "test part 2 2" in {
    part2(parseFile("2023/10-test-4.txt")) shouldEqual 8
  }

  it should "test part 2 3" in {
    part2(parseFile("2023/10-test-5.txt")) shouldEqual 10
  }

  it should "real part 2" ignore {
    part2(parseFile("2023/10.txt")) shouldEqual 351
  }
}
