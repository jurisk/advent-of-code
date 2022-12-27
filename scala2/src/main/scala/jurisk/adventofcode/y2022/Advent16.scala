package jurisk.adventofcode.y2022

import jurisk.algorithms.pathfinding.FloydWarshall
import jurisk.utils.FileInput.readFileText
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent16 {
  type ValveId = String

  final case class ValveDefinition(
    id: ValveId,
    flowRate: Int,
    leadsTo: Set[ValveId],
  )

  object ValveDefinition {
    def parse(s: String): ValveDefinition =
      s match {
        case s"Valve $id has flow rate=$flowRate; tunnels lead to valves $others" =>
          ValveDefinition(
            id,
            flowRate.toInt,
            others.split(", ").toSet,
          )
        case s"Valve $id has flow rate=$flowRate; tunnel leads to valve $other"   =>
          ValveDefinition(
            id,
            flowRate.toInt,
            Set(other),
          )
        case _                                                                    =>
          s.failedToParse
      }
  }

  def parse(data: String): Task = {
    val input                          = data.split("\n")
    val parsed: Array[ValveDefinition] = input.map { s =>
      ValveDefinition.parse(s)
    }

    val valves = parsed.toList.map { x =>
      x.id -> x
    }.toMap

    Task(valves)
  }

  final case class Task(valves: Map[ValveId, ValveDefinition]) {
    private val distancesToAll: Map[(ValveId, ValveId), Int] =
      FloydWarshall.allPairsDistances[ValveId, Int](
        valves.keySet.toList,
        (from, to) => if (valves(from).leadsTo.contains(to)) Some(1) else None,
      )

    def distance(from: ValveId, to: ValveId): Int = distancesToAll((from, to))

    private val usefulValves: Set[ValveId] =
      valves.values.filter(_.flowRate > 0).map(_.id).toSet

    def allWaysToOpen(
      from: ValveId,
      openValves: Vector[ValveId],
      timeLeft: Int,
    ): List[Vector[ValveId]] = {
      val candidateValves = usefulValves -- openValves.toSet
      val reachable       =
        candidateValves.filter(v => distance(from, v) < timeLeft)

      openValves :: reachable.toList.flatMap { next =>
        allWaysToOpen(
          next,
          openValves :+ next,
          timeLeft - distance(from, next) - 1,
        )
      }
    }

    @tailrec
    def score(
      curr: ValveId,
      wayToOpen: List[ValveId],
      timeLeft: Int,
      acc: Int = 0,
    ): Int =
      wayToOpen match {
        case Nil               => acc
        case next :: remaining =>
          val newTimeLeft = timeLeft - distance(curr, next) - 1
          score(
            next,
            remaining,
            newTimeLeft,
            acc + valves(next).flowRate * newTimeLeft,
          )
      }
  }

  private val Start = "AA"

  def part1(task: Task): Int = {
    val Time = 30
    val ways = task.allWaysToOpen(Start, Vector.empty, Time)

    ways.map(x => task.score(Start, x.toList, Time)).max
  }

  def part2(task: Task): Int = {
    val Time = 26
    val ways = task.allWaysToOpen(Start, Vector.empty, Time)

    val bestScoreForValveSet: Map[Set[ValveId], Int] = ways
      .map { way =>
        (way.toSet, task.score(Start, way.toList, Time))
      }
      .groupBy(_._1)
      .map { case (k, v) =>
        k -> v.map(_._2).max
      }

    val options = for {
      (humanSet, humanScore)       <- bestScoreForValveSet
      (elephantSet, elephantScore) <- bestScoreForValveSet
      if humanSet.intersect(elephantSet).isEmpty
    } yield humanScore + elephantScore

    options.max
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/16-test.txt")
    val realData = readFileText("2022/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 1651
    part1(real) shouldEqual 1701

    part2(test) shouldEqual 1707
    part2(real) shouldEqual 2455
  }
}
