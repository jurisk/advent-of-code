package jurisk.adventofcode.y2025

import jurisk.algorithms.pathfinding.AStar
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.utils.FileInput._
import jurisk.utils.Memoize
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
      final case class JoltageState(
        joltage: Joltage,
        minimumNextButtonIndex: Int,
      )

      def start: JoltageState =
        JoltageState(Joltage(ArraySeq.fill(indicatorLights.length)(0)), 0)

      def neighbours(state: JoltageState): Seq[JoltageState] =
        buttons.zipWithIndex
          .drop(state.minimumNextButtonIndex)
          .map { case (button, buttonIndex) =>
            JoltageState(state.joltage + button, buttonIndex)
          }
          .filter { nextState =>
            nextState.joltage.values.indices
              .forall(i =>
                nextState.joltage.values(i) <= joltageRequirements.values(i)
              )
          }

      val memoizedNeighbours = Memoize.memoize1(neighbours)

      def heuristic(state: JoltageState): Int =
        state.joltage.values.indices
          .map(i => joltageRequirements.values(i) - state.joltage.values(i))
          .max

      def isGoal(state: JoltageState): Boolean =
        state.joltage == joltageRequirements

      AStar.aStar[JoltageState, Int](
        start,
        s => memoizedNeighbours(s).map(n => (n, 1)),
        heuristic,
        isGoal,
      ) match {
        case Some((_, cost)) => cost
        case None            => "No solution found".fail
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
    data.zipWithIndex.map { case (machine, idx) =>
      println(s"Processing machine $idx / ${data.size}")
      machine.minimumButtonsToGetJoltageRequirements
    }.sum

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
