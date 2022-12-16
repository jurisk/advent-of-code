package jurisk.adventofcode.y2022

import cats.data.NonEmptyList
import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra, Pathfinding}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

object Advent16 {
  type Parsed = ValveDefinitions
  type ValveId = String

  final case class ValveDefinition(
    id: ValveId,
    flowRate: Int,
    leadsTo: Set[ValveId],
  )

  final case class State1(
    timeElapsed: Int,
    waterReleased: Int,
    location: ValveId,
    valvesOpen: Set[ValveId],
  ) {
    def currentWaterRate(definitions: ValveDefinitions): Int = {
      valvesOpen.map { v =>
        definitions(v).flowRate
      }.sum
    }

    def isFinished(maxTime: Int): Boolean = {
      timeElapsed > maxTime
    }

    def debug(definitions: ValveDefinitions): Unit = {
      println(s"== Minute $timeElapsed ==")
      if (valvesOpen.isEmpty) {
        println("No valves are open.")
      } else {
        println(s"Valves ${valvesOpen.toList.sorted.mkString(", ")} are open, releasing ${currentWaterRate(definitions)} pressure.")
      }
      println(s"You are at $location and $waterReleased water has been released so far.")
    }
  }

  final case class State2(
    timeElapsed: Int,
    waterReleased: Int,
    locations: Set[ValveId],
    valvesOpen: Set[ValveId],
  ) {
    def goFromBoth(definitions: ValveDefinitions, a: ValveId, b: ValveId): List[State2] = {
      val neighboursA = definitions(a).leadsTo.toList
      val neighboursB = definitions(b).leadsTo.toList
      for {
        an <- neighboursA
        bn <- neighboursB
      } yield State2(
        timeElapsed,
        waterReleased,
        Set(an, bn),
        valvesOpen,
      )
    }

    def openValveAndGoFrom(definitions: ValveDefinitions, valveToOpen: ValveId, goFrom: ValveId): List[State2] = {
      definitions(goFrom).leadsTo.toList.map { newLocation =>
        State2(
          timeElapsed,
          waterReleased,
          Set(valveToOpen, newLocation),
          valvesOpen + valveToOpen
        )
      }
    }

    def openTwoValves(a: ValveId, b: ValveId): State2 = State2(
      timeElapsed,
      waterReleased,
      Set(a, b),
      valvesOpen + a + b,
    )

    def rollTurn(definitions: ValveDefinitions): State2 = State2(
                    timeElapsed + 1,
                    waterReleased + currentWaterRate(definitions),
      locations,
      valvesOpen,
    )

    def currentWaterRate(definitions: ValveDefinitions): Int = {
      valvesOpen.map { v =>
        definitions(v).flowRate
      }.sum
    }

    def isFinished(maxTime: Int): Boolean = {
      timeElapsed > maxTime
    }

    val locationList: List[ValveId] = locations.toList.sorted match {
      case x :: Nil => x :: x :: Nil
      case a :: b :: Nil => a :: b :: Nil
      case _ => sys.error("wtf")
    }

    def debug(definitions: ValveDefinitions): Unit = {
      println(s"== Minute $timeElapsed ==")
      if (valvesOpen.isEmpty) {
        println("No valves are open.")
      } else {
        println(s"Valves ${valvesOpen.toList.sorted.mkString(", ")} are open, releasing ${currentWaterRate(definitions)} pressure.")
      }
      println(s"You are at $locations and $waterReleased water has been released so far.")
    }
  }

  object ValveDefinition {
    def parse(s: String): ValveDefinition = {
      s match {
        case s"Valve $id has flow rate=$flowRate; tunnels lead to valves $others" => ValveDefinition(
          id,
          flowRate.toInt,
          others.split(", ").toSet,
        )
        case s"Valve $id has flow rate=$flowRate; tunnel leads to valve $other" => ValveDefinition(
          id,
          flowRate.toInt,
          Set(other),
        )
        case _                 => s"Failed to parse $s".fail
      }
    }
  }

  type ValveDefinitions = Map[ValveId, ValveDefinition]
  final case class TaskDefinition(
    definitions: ValveDefinitions,
    maxTime: Int,
  ) {
    def successors1(state: State1): List[State1] = {
      if (state.isFinished(maxTime)) {
        Nil
      } else {
        if (definitions.keySet.size == state.valvesOpen.size) { // all valves open
          State1(
            state.timeElapsed + 1,
            state.waterReleased + state.currentWaterRate(definitions),
            state.location,
            state.valvesOpen,
          ) :: Nil
        } else {
          val maybeOpen: Option[State1] = if ((definitions(state.location).flowRate > 0) && !state.valvesOpen(state.location)) {
            State1(
              state.timeElapsed + 1,
              state.waterReleased + state.currentWaterRate(definitions),
              state.location,
              state.valvesOpen + state.location
            ).some
          } else {
            none
          }

          val whereCanGo: List[State1] = definitions(state.location).leadsTo.toList.map { x =>
            State1(
              state.timeElapsed + 1,
              state.waterReleased + state.currentWaterRate(definitions),
              x,
              state.valvesOpen,
            )
          }

          val results = maybeOpen.toList ::: whereCanGo
          results
        }
      }
    }

    def successors2(state: State2): List[State2] = {
      if (state.isFinished(maxTime)) {
        Nil
      } else {
        if (definitions.keySet.size == state.valvesOpen.size) { // all valves open
          State2(
            state.timeElapsed + 1,
            state.waterReleased + state.currentWaterRate(definitions),
            state.locations,
            state.valvesOpen,
          ) :: Nil
        } else {
          val List(a, b) = state.locationList
          val usefulValveAtA = (definitions(a).flowRate > 0) && !state.valvesOpen(a)
          val usefulValveAtB = (definitions(b).flowRate > 0) && !state.valvesOpen(b)

          val first: List[State2] = if (usefulValveAtA && usefulValveAtB) {
            state.rollTurn(definitions).openTwoValves(a, b) :: Nil
          } else Nil

          val second1: List[State2] = if (usefulValveAtA) {
            state.rollTurn(definitions).openValveAndGoFrom(definitions, a, b)
          } else Nil

          val second2 = if (usefulValveAtB) {
            state.rollTurn(definitions).openValveAndGoFrom(definitions, b, a)
          } else Nil

          val third: List[State2] = state.rollTurn(definitions).goFromBoth(definitions, a, b)

          first ::: second1 ::: second2 ::: third
        }
      }
    }

  }

  type Key1 = (Int, ValveId, Set[ValveId])
  var bestCache1: mutable.Map[Key1, State1] = mutable.Map.empty

  type Key2 = (Int, Set[ValveId], Set[ValveId])
  var bestCache2: mutable.Map[Key2, State2] = mutable.Map.empty

  def parse(data: String): Parsed = {
    data.parseList("\n", ValveDefinition.parse).map { x =>
      x.id -> x
    }.toMap
  }

  def part1(definitions: Parsed): Int = {
    println(definitions)
    val start = State1(
      1,
      0,
      "AA",
      Set.empty,
    )

    var best: Int = 0
    def visit(state: State1): Unit = {
      if (state.waterReleased > best) {
        best = state.waterReleased
      }
    }

    val taskDefinition = TaskDefinition(definitions, 30)

    def successors(state: State1): List[State1] = {
      val key = (state.timeElapsed, state.location, state.valvesOpen)
      val bestSoFar = bestCache1.get(key)
      bestSoFar match {
        case Some(x) =>
          if (x.waterReleased > state.waterReleased) {
            Nil
          } else {
            bestCache1.update(key, state)
            taskDefinition.successors1(state)
          }

        case None =>
          bestCache1.update(key, state)
          taskDefinition.successors1(state)
      }

    }

    Bfs.bfsVisitAll(start, successors, visit)

    best
  }

  def part2(definitions: Parsed): Int = {
    println(definitions)
    val start = State2(
      1,
      0,
      Set("AA"),
      Set.empty,
    )

    var best: Int = 0

    def visit(state: State2): Unit = {
      if (state.waterReleased > best) {
        best = state.waterReleased
      }
    }

    val taskDefinition = TaskDefinition(definitions, 26)

    def successors(state: State2): List[State2] = {
      val key: Key2 = (state.timeElapsed, state.locations, state.valvesOpen)
      val bestSoFar = bestCache2.get(key)
      bestSoFar match {
        case Some(x) =>
          if (x.waterReleased > state.waterReleased) {
            Nil
          } else {
//            println(s"Found $key ${state}")
//            state.debug(definitions)
//            println()
            bestCache2.update(key, state)
            taskDefinition.successors2(state)
          }

        case None =>
//          println(s"Found $key ${state}")
//          state.debug(definitions)
//          println()
          bestCache2.update(key, state)
          taskDefinition.successors2(state)
      }

    }

    Bfs.bfsVisitAll(start, successors, visit)

//    (1 to 27) foreach { n =>
//      println(s"Best in minute $n: ")
//      val result = bestCache2.filter(_._1._1 == n).values.maxBy(_.waterReleased)
//      result.debug(definitions)
//      println()
//    }

    best
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/16-test.txt")
    // val testData = """"""
    val realData = readFileText("2022/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 1651
    part1(real) shouldEqual 1701

    part2(test) shouldEqual 1707
    part2(real) shouldEqual 1234465
  }
}
