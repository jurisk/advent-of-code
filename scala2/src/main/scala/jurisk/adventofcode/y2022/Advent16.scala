package jurisk.adventofcode.y2022

import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.algorithms.pathfinding.Bfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.BitSet
import scala.collection.mutable

object Advent16 {
  type Parsed = ValveDefinitions
  type ValveId = Int
  type ValveIdSet = BitSet

  final case class ValveDefinition(
    id: ValveId,
    idString: String,
    flowRate: Int,
    leadsToString: Set[String],
    leadsTo: ValveIdSet,
  )

  final case class State1(
    timeElapsed: Int,
    waterReleased: Int,
    location: ValveId,
    valvesOpen: ValveIdSet,
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
    locations: ValveIdSet,
    valvesOpen: ValveIdSet,
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
        BitSet.fromSpecific(an :: bn :: Nil),
        valvesOpen,
      )
    }

    def openValveAndGoFrom(definitions: ValveDefinitions, valveToOpen: ValveId, goFrom: ValveId): List[State2] = {
      definitions(goFrom).leadsTo.toList.map { newLocation =>
        State2(
          timeElapsed,
          waterReleased,
          BitSet.fromSpecific(valveToOpen :: newLocation :: Nil),
          valvesOpen + valveToOpen
        )
      }
    }

    def openTwoValves(a: ValveId, b: ValveId): State2 = State2(
      timeElapsed,
      waterReleased,
      BitSet.fromSpecific(a :: b :: Nil),
      valvesOpen + a + b,
    )

    def rollTurn(definitions: ValveDefinitions): State2 =
      State2(
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
    def parse(id: Int, s: String): ValveDefinition = {
      s match {
        case s"Valve $idString has flow rate=$flowRate; tunnels lead to valves $others" => ValveDefinition(
          id,
          idString,
          flowRate.toInt,
          others.split(", ").toSet,
          BitSet.empty,
        )
        case s"Valve $idString has flow rate=$flowRate; tunnel leads to valve $other" => ValveDefinition(
          id,
          idString,
          flowRate.toInt,
          Set(other),
          BitSet.empty,
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

          (first ::: second1 ::: second2 ::: third).distinct.sortBy(_.valvesOpen.size)(Ordering[Int].reverse)
        }
      }
    }

  }

  // (time, location, openValves)
  type Key1 = (Int, ValveId, ValveIdSet)
  var bestCache1: mutable.Map[Key1, State1] = mutable.Map.empty

  // (time, locations, openValves)
  type Key2 = (Int, ValveIdSet, ValveIdSet)
  var bestCache2: mutable.Map[Key2, State2] = mutable.Map.empty

  def parse(data: String): Parsed = {
    val input = data.split("\n")
    val parsed: Array[ValveDefinition] = input.zipWithIndex.map { case (s, idx) =>
      ValveDefinition.parse(idx, s)
    }

    parsed.toList.map { x =>
      x.id -> x.copy(
        leadsTo = BitSet.fromSpecific(
          x.leadsToString.map(s => parsed.find(_.idString == s).get.id)
        )
      )
    }.toMap
  }

  def part1(definitions: Parsed): Int = {
    val startValve = definitions.values.find(_.idString == "AA").get.id

    println(definitions)
    val start = State1(
      1,
      0,
      startValve,
      BitSet.empty,
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
    val startValve = definitions.values.find(_.idString == "AA").get.id

    val start = State2(
      1,
      0,
      BitSet.fromSpecific(startValve :: Nil),
      BitSet.empty,
    )

    var best: Int = 0

    def visit(state: State2): Unit = {
      if (state.waterReleased > best) {
        best = state.waterReleased
        println(s"Found better $best at $state")
      }
    }

    val taskDefinition = TaskDefinition(definitions, 26)

    def successors(state: State2): List[State2] = {
      val key: Key2 = (state.timeElapsed, state.locations, state.valvesOpen)
      val bestAtThisSituation = bestCache2.get(key).map(_.waterReleased).getOrElse(0)

      if (bestAtThisSituation > state.waterReleased) {
        Nil // prune
      } else {
        if (bestCache2.size % 10000 == 0) {
          println(bestCache2.size)
        }
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

    println(s"Found $best, the cache size is ${bestCache2.size}")
    best
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/16-test.txt")
    // val testData = """"""
    val realData = readFileText("2022/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    println(real.keySet)

//    part1(test) shouldEqual 1651
//    part1(real) shouldEqual 1701
//
    part2(test) shouldEqual 1707
    part2(real) shouldEqual 1234465
  }
}
