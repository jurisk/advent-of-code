package jurisk.adventofcode.y2025

import com.microsoft.z3.IntExpr
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.optimization.ImplicitConversions.RichArithExprIntSort
import jurisk.optimization.ImplicitConversions.RichExpr
import jurisk.optimization.ImplicitConversions.RichInt
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

object Advent10 {
  type Input           = List[Machine]
  private type LightId = Int
  type N               = Int

  private type IndicatorLightState = ArraySeq[Boolean]

  final case class Joltage(values: ArraySeq[Int]) {
    def +(indices: ArraySeq[Int]): Joltage =
      Joltage(indices.foldLeft(values) { case (state, idx) =>
        state.updated(idx, state(idx) + 1)
      })
  }

  final case class Machine(
    indicatorLights: IndicatorLightState,
    buttons: ArraySeq[ArraySeq[LightId]],
    joltageRequirements: Joltage,
  ) {
    def minimumButtonsToGetIndicatorLights: Int = {
      def start: IndicatorLightState =
        ArraySeq.fill(indicatorLights.length)(false)

      def successors(
        state: IndicatorLightState
      ): Seq[IndicatorLightState] =
        buttons.map { button =>
          button.foldLeft(state) { case (currentState, lightId) =>
            currentState.updated(lightId, !currentState(lightId))
          }
        }

      def success(
        state: IndicatorLightState
      ): Boolean =
        state == indicatorLights

      Dijkstra.dijkstraWithIdenticalCosts[IndicatorLightState, Int](
        start,
        successors,
        success,
      ) match {
        case Some((_, b)) => b
        case None         => "No solution found".fail
      }
    }

    def minimumButtonsToGetJoltageRequirements: Int = {
      implicit val optimizer: Optimizer = Optimizer.z3()
      import optimizer._

      // Create a variable for each button: how many times it's pressed
      val buttonVars: ArraySeq[IntExpr] = buttons.indices
        .map { i =>
          labeledInt(s"b$i")
        }
        .to(ArraySeq)

      // Each button press count must be non-negative
      buttonVars.foreach { v =>
        addConstraints(v >= Zero)
      }

      // For each light, the sum of button presses affecting it must equal the requirement
      val numLights = joltageRequirements.values.length
      for (lightIdx <- 0 until numLights) {
        val requirement         = joltageRequirements.values(lightIdx)
        // Find all buttons that affect this light
        val contributingButtons = buttons.indices.filter { buttonIdx =>
          buttons(buttonIdx).contains(lightIdx)
        }

        val lightSum = sum(contributingButtons.map(buttonVars): _*)
        addConstraints(lightSum === requirement.constant)
      }

      // Minimize total button presses
      val cost = labeledInt("cost")
      addConstraints(cost === sum(buttonVars: _*))
      val _    = minimize(cost)

      runExternal("cost") match {
        case Some(results) =>
          resultToInt(results.head)
        case None          =>
          "No solution found".fail
      }
    }
  }

  object Machine {
    private val LinePattern   = """^\[([.#]+)]\s+(.+)\s+\{([^}]+)}$""".r
    private val ButtonPattern = """\(([^)]*)\)""".r

    def parse(line: String): Machine = line match {
      case LinePattern(lights, buttonsStr, joltage) =>
        val indicatorLights     = ArraySeq.from(lights.map(_ == '#'))
        val buttons             = ArraySeq.from(
          ButtonPattern
            .findAllMatchIn(buttonsStr)
            .map { m =>
              val ids = m.group(1)
              ArraySeq.from(
                if (ids.isEmpty) Seq.empty
                else ids.split(",").map(_.toInt)
              )
            }
            .toSeq
        )
        val joltageRequirements = Joltage(
          ArraySeq.from(joltage.split(",").map(_.toInt))
        )
        Machine(indicatorLights, buttons, joltageRequirements)

      case _ =>
        s"Cannot parse line: $line".fail
    }
  }

  def parse(input: String): Input =
    input.parseLines(Machine.parse)

  def part1(data: Input): N =
    data.map(_.minimumButtonsToGetIndicatorLights).sum

  def part2(data: Input): N =
    data.map(_.minimumButtonsToGetJoltageRequirements).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/10$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
