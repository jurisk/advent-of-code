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
    def -(indices: ArraySeq[Int]): Option[Joltage] = {
      val result = indices.foldLeft(values) { case (state, idx) =>
        state.updated(idx, state(idx) - 1)
      }
      if (result.forall(_ >= 0)) Some(Joltage(result)) else None
    }
  }

  object Joltage {
    def zero(length: Int): Joltage = Joltage(ArraySeq.fill(length)(0))
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
      val goal = Joltage.zero(indicatorLights.length)
      val sortedButtons = buttons.sortBy(-_.length)
      var callCount = 0

      def f(state: Joltage): Option[Int] = {
        callCount += 1
        if (callCount % 100_000 == 0) {
          println(s"Call count: $callCount, state: $state")
        }
        if (state == goal) Some(0)
        else {
          val results = sortedButtons.flatMap { button =>
            val prev = state - button
            prev.flatMap(p => memoizedF(p).map(_ + 1))
          }
          if (results.isEmpty) None else Some(results.min)
        }
      }

      lazy val memoizedF: Joltage => Option[Int] = Memoize.memoize1(f)

      memoizedF(joltageRequirements).getOrElse("No solution found".fail)
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
