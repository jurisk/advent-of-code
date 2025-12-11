package jurisk.adventofcode.y2025

import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

object Advent10 {
  type Input           = List[Machine]
  private type LightId = Int
  type N               = Int

  private type IndicatorLightState = ArraySeq[Boolean]
  final case class Joltage(joltages: ArraySeq[Int]) {
    def +(indices: ArraySeq[Int]): Joltage =
      Joltage(indices.foldLeft(joltages) { case (state, idx) =>
        state.updated(idx, state(idx) + 1)
      })
  }

  final case class Machine(
    indicatorLights: IndicatorLightState,
    buttons: ArraySeq[ArraySeq[LightId]],
    joltageRequirements: Joltage,
  ) {
    private val buttonsJoltage: ArraySeq[Joltage] =
      buttons.map { button =>
        Joltage {
          button.map(lightId => if (indicatorLights(lightId)) 1 else 0)
        }
      }

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
      def isValid(joltage: Joltage): Boolean =
        joltage.joltages.indices.forall(i =>
          joltage.joltages(i) <= joltageRequirements.joltages(i)
        )

      def f(current: Joltage): Option[Int] =
        if (current == joltageRequirements) Some(0)
        else {
          val results = buttons.indices.flatMap { buttonIndex =>
            val newJoltage = current + buttons(buttonIndex)
            if (isValid(newJoltage)) f(newJoltage).map(_ + 1)
            else None
          }
          if (results.isEmpty) None else Some(results.min)
        }

      f(Joltage(ArraySeq.fill(indicatorLights.length)(0))).getOrElse(
        "No solution found".fail
      )
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
        val joltageRequirements = ArraySeq.from(joltage.split(",").map(_.toInt))
        Machine(indicatorLights, buttons, Joltage(joltageRequirements))

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
