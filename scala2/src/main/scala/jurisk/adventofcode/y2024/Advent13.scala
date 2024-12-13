package jurisk.adventofcode.y2024

import jurisk.adventofcode.y2024.Advent13.SolutionMode.BruteForce
import jurisk.adventofcode.y2024.Advent13.SolutionMode.LinearEquations
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3Mode.External
import jurisk.adventofcode.y2024.Advent13.SolutionMode.Z3Mode.Internal
import jurisk.geometry.Coordinates2D
import jurisk.math.LinearEquationSystems
import jurisk.optimization.ImplicitConversions.RichArithExprIntSort
import jurisk.optimization.ImplicitConversions.RichExpr
import jurisk.optimization.ImplicitConversions.RichLong
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.nowarn

object Advent13 {
  type Input = List[Machine]
  type N     = Long
  type C     = Coordinates2D[Long]

  sealed trait SolutionMode
  object SolutionMode {
    final case class BruteForce(limit: Int) extends SolutionMode
    final case class Z3(mode: Z3Mode)       extends SolutionMode
    case object LinearEquations       extends SolutionMode

    sealed trait Z3Mode
    object Z3Mode {
      case object Internal extends Z3Mode
      case object External extends Z3Mode
    }
  }

  private val CostA = 3
  private val CostB = 1

  final case class Machine(
    buttonA: C,
    buttonB: C,
    prize: C,
  ) {
    def solve(
      solutionMode: SolutionMode
    ): Option[N] =
      solutionMode match {
        case BruteForce(limit) => bruteForceConstrained(limit)
        case Z3(mode)          => solveZ3(mode)
        case LinearEquations   => linearEquations
      }

    private def linearEquations: Option[N] =
      LinearEquationSystems
        .solveTwoVariablesInteger(
          buttonA.x,
          buttonB.x,
          buttonA.y,
          buttonB.y,
          prize.x,
          prize.y,
        )
        .map { case (a, b) =>
          CostA * a + CostB * b
        }

    private def bruteForceConstrained(limit: Int = 100): Option[N] = {
      val results = for {
        a     <- 0L to limit
        b     <- 0L to limit
        result = buttonA * a + buttonB * b
        if result == prize
      } yield CostA * a + CostB * b

      results.minOption
    }

    private def solveZ3(
      z3Mode: SolutionMode.Z3Mode
    ): Option[N] = {
      println(s"Solving $this")

      // a * ax + b * bx = px
      // a * ay + b * by = py
      // Minimize 3 * a + b

      implicit val optimizer: Optimizer = Optimizer.z3()
      import optimizer._

      val List(a, b, cost) = List("a", "b", "cost").map(labeledInt)
      val (ax, bx, px)     =
        (buttonA.x.constant, buttonB.x.constant, prize.x.constant)
      val (ay, by, py)     =
        (buttonA.y.constant, buttonB.y.constant, prize.y.constant)

      addConstraints(
        a >= Zero,
        b >= Zero,
        a * ax + b * bx === px,
        a * ay + b * by === py,
        cost === CostA.constant * a + CostB.constant * b,
      )

      val _ = minimize(cost)

      z3Mode match {
        case Internal =>
          @nowarn("cat=deprecation")
          val modelResult = checkAndGetModel()
          modelResult.map { m =>
            m.getConstInterp(cost).toString.toLong
          }

        case External =>
          runExternal("cost").map { m =>
            val List(cost) = m.map(resultToLong)
            cost
          }
      }
    }
  }

  private object Machine {
    def parse(s: String): Machine = {
      def parseCoords(s: String, symbol: String): C = {
        val Pattern = s"""X$symbol(\\d+), Y$symbol(\\d+)""".r
        s match {
          case Pattern(x, y) => Coordinates2D(x.toLong, y.toLong)
          case _             => s.fail
        }
      }

      val List(as, bs, ps) = s.split("\n").toList
      val a                = as.removePrefixUnsafe("Button A: ")
      val b                = bs.removePrefixUnsafe("Button B: ")
      val p                = ps.removePrefixUnsafe("Prize: ")
      Machine(parseCoords(a, "\\+"), parseCoords(b, "\\+"), parseCoords(p, "="))
    }
  }

  def parse(input: String): Input =
    input.parseSections(Machine.parse)

  def part1(data: Input): N =
    data.flatMap(_.solve(BruteForce(100))).sum

  def part2(data: Input): N = {
    val adjusted = data.map(m =>
      m.copy(prize = m.prize + Coordinates2D(10000000000000L, 10000000000000L))
    )

    adjusted.flatMap(_.solve(LinearEquations)).sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/13$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
