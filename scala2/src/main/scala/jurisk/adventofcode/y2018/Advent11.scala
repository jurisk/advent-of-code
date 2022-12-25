package jurisk.adventofcode.y2018

import jurisk.geometry.Coords2D
import org.scalatest.matchers.should.Matchers._

object Advent11 {
  private def totalPower(serial: Int, x: Int, y: Int): Int = {
    // Find the rack ID, which is the X coordinate plus 10.
    val rackId = x + 10

    // Begin with a power level of the rack ID times the Y coordinate.
    val power1 = rackId * y

    // Increase the power level by the value of the grid serial number.
    val power2 = power1 + serial

    // Set the power level to itself multiplied by the rack ID.
    val power3 = power2 * rackId

    // Keep only the hundreds digit of the power level.
    // If the power level is less than 100, set it to 0.
    val power4 = if (power3 < 100) 0 else (power3 / 100) % 10

    // Subtract 5 from the power level.
    power4 - 5
  }

  private val GridSize                             = 300
  def solve1(serial: Int, N: Int): (Coords2D, Int) = {
    val grid: Array[Array[Int]] = (1 to GridSize).toArray map { y =>
      (1 to GridSize).toArray map { x =>
        totalPower(serial, x, y)
      }
    }

    val coords = (0 until GridSize + 1 - N) flatMap { y =>
      (0 until GridSize + 1 - N) map { x =>
        Coords2D.of(x, y)
      }
    }

    val (bestCoords, bestPower) = coords
      .map { c =>
        val power = (0 until N).map { i =>
          grid(c.y + i)
            .slice(c.x, c.x + N)
            .sum
        }.sum

        (c + Coords2D.of(1, 1)) -> power
      }
      .maxBy { case (_, power) => power }

    println(
      s"Max power for size $N is at $bestCoords with power of $bestPower"
    )
    (bestCoords, bestPower)
  }

  def solve2(serial: Int): (Coords2D, Int) = {
    val (bestCoords, _, bestSize) = (1 to GridSize)
      .map { size =>
        val (bestCoords, result) = solve1(serial, size)
        (bestCoords, result, size)
      }
      .maxBy { case (_, result, _) => result }

    println(s"The best found for serial $serial is $bestCoords $bestSize")

    (bestCoords, bestSize)
  }

  def main(args: Array[String]): Unit = {
    totalPower(8, 3, 5) shouldEqual 4
    totalPower(57, 122, 79) shouldEqual -5
    totalPower(39, 217, 196) shouldEqual 0
    totalPower(71, 101, 153) shouldEqual 4

    solve1(18, 3) shouldEqual (Coords2D.of(33, 45), 29)
    solve1(42, 3) shouldEqual (Coords2D.of(21, 61), 30)
    solve1(3214, 3) shouldEqual (Coords2D.of(21, 42), 32)

    solve2(18) shouldEqual (Coords2D.of(90, 269), 16)
    solve2(42) shouldEqual (Coords2D.of(232, 251), 12)

    solve2(3214) shouldEqual (Coords2D.of(230, 212), 13)
  }
}
