package jurisk.math

import jurisk.math.GaussianElimination.solve
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class GaussianEliminationSpec extends AnyFreeSpec {
  private def compare(
    solution: Array[Double],
    expected: Array[Double],
  ): Unit = {
    val Eps = 1e-5

    solution.length shouldEqual expected.length
    for ((v, i) <- solution.zipWithIndex)
      assert(
        math.abs(v - expected(i)) < Eps,
        s"Value at index $i should be close to ${expected(i)}",
      )
  }

  "GaussianElimination" - {
    "test 1" in {
      // 2x + y -z = 8
      // -3x -y + 2z = -11
      // -2x +y + 2z = -3
      val A        = Array(
        Array(2.0, 1.0, -1.0),
        Array(-3.0, -1.0, 2.0),
        Array(-2.0, 1.0, 2.0),
      )
      val b        = Array(8.0, -11.0, -3.0)
      val expected = Array(2.0, 3.0, -1.0)
      val solution = solve(A, b)

      compare(solution, expected)
    }

    // https://en.wikipedia.org/wiki/Gaussian_elimination#Example_of_the_algorithm
    "test from Wikipedia" in {
      // 2x + y - z = 8
      // -3x -y + 2z = -11
      // -2x + y + 2z = -3
      val A = Array(
        Array(2.0, 1.0, -1.0),
        Array(-3.0, -1.0, 2.0),
        Array(-2.0, 1.0, 2.0),
      )

      val b        = Array(8.0, -11.0, -3.0)
      val expected = Array(2.0, 3.0, -1.0)
      val solution = solve(A, b)

      compare(solution, expected)
    }
  }
}
