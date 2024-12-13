package jurisk.math

import org.scalatest.freespec.AnyFreeSpec

class LinearEquationSystemsSpec extends AnyFreeSpec {
  "solveTwoVariablesInteger" - {
    "should solve a system of two linear equations with two variables" in {
      assert(
        LinearEquationSystems.solveTwoVariablesInteger(2, 1, 3, -1, 15,
          5) === Some((4, 7))
      )
    }

    "should return None if the system of two linear equations with two variables has no solution" in {
      assert(
        LinearEquationSystems.solveTwoVariablesInteger(1, 2, 1, 2, 5,
          6) === None
      )
    }
  }
}
