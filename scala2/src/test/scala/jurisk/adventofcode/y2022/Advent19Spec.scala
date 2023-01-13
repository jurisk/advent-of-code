package jurisk.adventofcode.y2022

import cats.effect.testing.scalatest.AsyncIOSpec
import jurisk.adventofcode.y2022.Advent19._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class Advent19Spec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "Solution 19" should "calculate successors" in {
    testData.asserting { test =>
      val test1 = test.head

      State.Start shouldEqual State(oreRobots = 1)
      val m1 = State(minutesPassed = 1, oreRobots = 1, ore = 1)
      State.Start.successors(test1) shouldEqual m1 :: Nil
      val m2 = State(minutesPassed = 2, oreRobots = 1, ore = 2)
      m1.successors(test1) shouldEqual m2 :: Nil

      val m3 = State(minutesPassed = 3, oreRobots = 1, clayRobots = 1, ore = 1)
      m2.successors(test1) should contain(m3)

      val m4 =
        State(
          minutesPassed = 4,
          oreRobots = 1,
          clayRobots = 1,
          ore = 2,
          clay = 1,
        )
      m3.successors(test1) should contain(m4)

      val m5 =
        State(
          minutesPassed = 5,
          oreRobots = 1,
          clayRobots = 2,
          ore = 1,
          clay = 2,
        )
      m4.successors(test1) should contain(m5)
    }
  }

  it should "test 1 part 1" in {
    testData.asserting { test =>
      test.head.geodesCanOpen(Part1Minutes) shouldEqual 9
    }
  }

  it should "test 2 part 1" in {
    testData.asserting { test =>
      test(1).geodesCanOpen(Part1Minutes) shouldEqual 12
    }
  }

  it should "part 1 test" in {
    for {
      test   <- testData
      result <- part1(test)
    } yield result shouldEqual 33
  }

  it should "part 1 real" ignore {
    for {
      real   <- realData
      result <- part1(real)
    } yield result shouldEqual 1624
  }

  it should "part 2 test 1" in {
    testData.asserting { test =>
      test.head.geodesCanOpen(Part2Minutes) shouldEqual 56
    }
  }

  it should "part 2 test 2" in {
    testData.asserting { test =>
      test(1).geodesCanOpen(Part2Minutes) shouldEqual 62
    }
  }

  it should "part 2 real" ignore {
    for {
      real   <- realData
      result <- part2(real.take(3))
    } yield result shouldEqual 12628
  }
}
