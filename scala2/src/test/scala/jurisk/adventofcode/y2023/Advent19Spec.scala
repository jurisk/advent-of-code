package jurisk.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import Advent19._
import org.scalatest.matchers.should.Matchers._

class Advent19Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 19114
    }

    "real" in {
      part1(realData) shouldEqual 377025
    }
  }

  "part 2" - {
    "only 1 in A dimension" in {
      part2(
        Input(
          workflows = Map(
            "in" -> Workflow(
              Criterion.LessThan(Dimension.A, 2, "A") ::
                Criterion.Forward("R")
                :: Nil
            )
          ),
          parts = Nil,
        )
      ) shouldEqual 1L * 4000 * 4000 * 4000
    }

    "test" in {
      part2(testData) shouldEqual 167409079868000L
    }

    "real" in {
      part2(realData) shouldEqual 135506683246673L
    }
  }
}
