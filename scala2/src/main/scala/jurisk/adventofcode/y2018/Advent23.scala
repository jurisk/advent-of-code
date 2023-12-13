package jurisk.adventofcode.y2018

import com.google.ortools.Loader
import com.google.ortools.sat.{
  CpModel,
  CpSolver,
  CpSolverStatus,
  LinearArgument,
  LinearExpr,
}
import jurisk.geometry.Coords3D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import java.util.Arrays.stream

object Advent23 {
  type Input = List[Nanobot]

  final case class Nanobot(
    position: Coords3D,
    radius: Int,
  )

  object Nanobot {
    def parse(s: String): Nanobot = {
      val RegEx = """pos=<([-+]?\d+),([-+]?\d+),([-+]?\d+)>, r=(\d+)""".r

      s match {
        case RegEx(x, y, z, r) =>
          Nanobot(Coords3D(x.toInt, y.toInt, z.toInt), r.toInt)
        case _                 => s.failedToParse
      }
    }
  }

  def parse(input: String): Input =
    input.parseLines(Nanobot.parse)

  def part1(data: Input): Int = {
    val strongest = data.maxBy(_.radius)
    data.count(x =>
      (x.position manhattanDistance strongest.position) <= strongest.radius
    )
  }

  def part2(data: Input): Int = {
    /*

    def zabs(x: int):
        return If(x >= 0, x, -x)

    def boolean_to_int(x):
        return If(x, 1, 0)

    def range_query(nanobot: Nanobot):
        (nx, ny, nz, nr) = (nanobot.location.x, nanobot.location.y, nanobot.location.z, nanobot.radius)
        return zabs(x - nx) + zabs(y - ny) + zabs(z - nz) <= nr
     */

    // nanobotInRange is array of Ints, and is equal to boolean_to_int(range_query(nanobots[idx]))
    // nanobotsInRange is sum of nanobotInRange

    // distanceFromOrigin is x.abs + y.abs + z.abs

    // Find x, y, z such that maximize nanobotsInRange and minimize distanceFromOrigin

    Loader.loadNativeLibraries()

    // Create the model.
    val model = new CpModel

    // Create the variables.
    val x = model.newIntVar(0, 1000, "x")
    val y = model.newIntVar(0, 1000, "y")
    val z = model.newIntVar(0, 1000, "z")

    // Create the constraints.
    // 2x + 7y + 3z <= 25
    model.addLessOrEqual(
      LinearExpr.weightedSum(Array(x, y, z), Array(2, 7, 3)),
      50,
    )

    model.addLessOrEqual(
      LinearExpr.weightedSum(Array(x, y, z), Array(3, -5, 7)),
      45,
    )
    model.addLessOrEqual(
      LinearExpr.weightedSum(Array(x, y, z), Array(5, 2, -6)),
      37,
    )

    // Objective - maximize 2x + 2y + 3z
    model.maximize(LinearExpr.weightedSum(Array(x, y, z), Array(2, 2, 3)))

    // Create a solver and solve the model.
    val solver = new CpSolver

    val status = solver.solve(model)

    if (
      (status eq CpSolverStatus.OPTIMAL) || (status eq CpSolverStatus.FEASIBLE)
    ) {
      println(s"Maximum of objective function: ${solver.objectiveValue}")
      println(s"x = ${solver.value(x)}")
      println(s"y = ${solver.value(y)}")
      println(s"z = ${solver.value(z)}")
    } else System.out.println("No solution found.")

    // Statistics.
    println("Statistics:")
    println(s"  conflicts: ${solver.numConflicts}")
    println(s"  branches : ${solver.numBranches}")
    println(s"  wall time: ${solver.wallTime}")

    0
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2018/23.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
