package jurisk.adventofcode.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import Advent06._

class Advent06Spec extends AnyFlatSpec {
  private val Test1: Input = Race(7, 9) :: Race(15, 40) :: Race(30, 200) :: Nil
  private val Test2: Input = convertInput(Test1)

  "Advent06" should "test part 1" in {
    solve(Test1) shouldEqual 288
  }

  it should "work for 7, 9" in {
    Race(7, 9).waysToWinAlgebraic shouldEqual 4
  }

  it should "work for 15, 40" in {
    Race(15, 40).waysToWinAlgebraic shouldEqual 8
  }

  it should "work for 30, 200" in {
    Race(30, 200).waysToWinAlgebraic shouldEqual 9
  }

  it should "real part 1" in {
    solve(Real1) shouldEqual 281600
  }

  it should "test part 2" in {
    solve(Test2) shouldEqual 71503
  }

  it should "real part 2" in {
    solve(Real2) shouldEqual 33875953
  }
}
