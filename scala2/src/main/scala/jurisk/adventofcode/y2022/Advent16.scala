package jurisk.adventofcode.y2022

import cats.implicits.{catsSyntaxOptionId, none}
import jurisk.algorithms.pathfinding.Bfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.BitSet
import scala.collection.mutable

object Advent16 {
  type Parsed     = ValveDefinitions
  type ValveId    = Int
  type ValveIdSet = BitSet

  final case class ValveDefinition(
    id: ValveId,
    idString: String,
    flowRate: Int,
    leadsToString: Set[String],
    leadsTo: ValveIdSet,
  )

  final case class State2(
    timeElapsed: Int,
    waterReleased: Int,
    locations: List[
      (ValveId, Option[ValveId]) // (location, destinationWithIntentionToOpen)
    ],
    valvesOpen: ValveIdSet,
  ) {
    def variousOptions(
      actorWithIntention: (ValveId, Option[ValveId]),
      definition: TaskDefinition,
    ): List[(ValveId, Option[ValveId], Option[ValveId])] = {
      val (currentLocation, intention) = actorWithIntention
      val result                       = intention match {
        case Some(whereWeWantToGo) =>
          if (whereWeWantToGo == currentLocation) { // we wanted to open this valve so we do it
            (currentLocation, None, Some(currentLocation)) :: Nil
          } else { // follow our intention
            val nextStep =
              definition.howDoWeGoTo((currentLocation, whereWeWantToGo))
            // println(s"Next step of going from ${definition.nodeName(currentLocation)} to ${definition.nodeName(whereWeWantToGo)} is ${definition.nodeName(nextStep)}")
            (nextStep, Some(whereWeWantToGo), None) :: Nil
          }

        case None =>
          val valvesStillClosed: ValveIdSet =
            definition.usefulValves -- valvesOpen
          valvesStillClosed.toList map { whereWeWantToGo =>
            val firstStep =
              definition.howDoWeGoTo((currentLocation, whereWeWantToGo))
            // println(s"Next step of going from ${definition.nodeName(currentLocation)} to ${definition.nodeName(whereWeWantToGo)} is ${definition.nodeName(firstStep)}")
            (firstStep, Some(whereWeWantToGo), None)
          }
      }
      result
    }

    def rollTurn(definitions: ValveDefinitions): State2 =
      State2(
        timeElapsed + 1,
        waterReleased + currentWaterRate(definitions),
        locations,
        valvesOpen,
      )

    def currentWaterRate(definitions: ValveDefinitions): Int =
      valvesOpen.map { v =>
        definitions(v).flowRate
      }.sum

    def isFinished(maxTime: Int): Boolean =
      timeElapsed > maxTime

    def debug(definition: TaskDefinition): Unit = {
      println(s"== Minute $timeElapsed ==")
      if (valvesOpen.isEmpty) {
        println("No valves are open.")
      } else {
        println(
          s"Valves ${valvesOpen.map(definition.nodeName).toList.sorted.mkString(", ")} are open, releasing ${currentWaterRate(definition.definitions)} pressure."
        )
      }
      println(s"You are at ${locations
          .map { case (a, b) =>
            definition.nodeName(a) + " intending for " + (b.map(definition.nodeName))
          }
          .mkString(", ")} and $waterReleased water has been released so far.")
    }
  }

  object ValveDefinition {
    def parse(id: Int, s: String): ValveDefinition =
      s match {
        case s"Valve $idString has flow rate=$flowRate; tunnels lead to valves $others" =>
          ValveDefinition(
            id,
            idString,
            flowRate.toInt,
            others.split(", ").toSet,
            BitSet.empty,
          )
        case s"Valve $idString has flow rate=$flowRate; tunnel leads to valve $other"   =>
          ValveDefinition(
            id,
            idString,
            flowRate.toInt,
            Set(other),
            BitSet.empty,
          )
        case _                                                                          => s"Failed to parse $s".fail
      }
  }

  type ValveDefinitions = Map[ValveId, ValveDefinition]
  final case class TaskDefinition(
    definitions: ValveDefinitions,
    maxTime: Int,
  ) {
    def nodeName(x: ValveId): String = definitions(x).idString

    val howDoWeGoTo: Map[(ValveId, ValveId), ValveId] = {
      val results: Iterable[(ValveId, ValveId, Option[ValveId])] = for {
        from <- definitions.keys
        to   <- definitions.keys
        if from != to
      } yield (
        from,
        to,
        Bfs
          .bfs(
            from,
            (n: ValveId) => definitions(n).leadsTo.toList,
            (n: ValveId) => n == to,
          )
          .map(x => x.tail.head),
      )

      results.flatMap { case (from, to, q) =>
        q map { q =>
          (from, to) -> q
        }
      }.toMap
    }

    def addToBitSet(bitSet: ValveIdSet, opt: Option[ValveId]): ValveIdSet =
      opt match {
        case None    => bitSet
        case Some(x) => bitSet + x
      }

    def successors2(state: State2): List[State2] =
      if (state.isFinished(maxTime)) {
        Nil
      } else {
        if (state.valvesOpen.size == usefulValves.size) { // all valves open
          state.rollTurn(definitions) :: Nil
        } else {
          val rolled    = state.rollTurn(definitions)
          val locations = state.locations

          locations match {
            case x :: Nil      =>
              rolled.variousOptions(x, this).map {
                opt: (ValveId, Option[ValveId], Option[ValveId]) =>
                  val (a1, a2, a3) = opt
                  rolled
                    .copy(locations = (a1, a2) :: Nil)
                    .copy(valvesOpen = addToBitSet(rolled.valvesOpen, a3))
              }
            case a :: b :: Nil =>
              val optionsForA = rolled.variousOptions(a, this)
              val optionsForB = rolled.variousOptions(b, this)

              // TODO: I think we can have conflicting stuff here, perhaps if both try to open the same valve then let us skip?
              optionsForA.flatMap { ao =>
                val (ao1, ao2, ao3) = ao
                optionsForB.flatMap { bo =>
                  val (bo1, bo2, bo3) = bo
                  if (ao3.isDefined && ao3 == bo3) {
                    none
                  } else {
                    rolled
                      .copy(locations = (ao1, ao2) :: (bo1, bo2) :: Nil)
                      .copy(valvesOpen =
                        addToBitSet(addToBitSet(rolled.valvesOpen, ao3), bo3)
                      )
                      .some
                  }
                }
              }

            case _ => sys.error("wtf")
          }
        }
      }

    val usefulValves: ValveIdSet =
      BitSet.fromSpecific(definitions.values.filter(_.flowRate > 0).map(_.id))
  }

  def parse(data: String): Parsed = {
    val input                          = data.split("\n")
    val parsed: Array[ValveDefinition] = input.zipWithIndex.map {
      case (s, idx) =>
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

    val start = State2(
      1,
      0,
      (startValve, None) :: Nil,
      BitSet.empty,
    )

    solve(definitions, start, 30)
  }

  def part2(definitions: Parsed): Int = {
    val startValve = definitions.values.find(_.idString == "AA").get.id

    val start = State2(
      1,
      0,
      (startValve, None) :: (startValve, None) :: Nil,
      BitSet.empty,
    )

    solve(definitions, start, 26)
  }

  def solve(definitions: Parsed, start: State2, maxTime: Int): Int = {
    // println(definitions)

    var best: Int = 0

    def visit(state: State2): Unit =
      if (state.waterReleased > best) {
        best = state.waterReleased
        println(s"Found better $best at $state")
      }

    val taskDefinition = TaskDefinition(definitions, maxTime)

    // (time, locations, openValves)
    type Key2 = (Int, ValveIdSet, ValveIdSet)
    val bestCache2: mutable.Map[Key2, State2] = mutable.Map.empty

    def successors(state: State2): List[State2] = {
      val key: Key2           = (
        state.timeElapsed,
        BitSet.fromSpecific(state.locations.map(_._1)),
        state.valvesOpen,
      )
      val bestAtThisSituation =
        bestCache2.get(key).map(_.waterReleased).getOrElse(0)

      if (bestAtThisSituation > state.waterReleased) {
        Nil // prune
      } else {
        if (bestCache2.size % 100000 == 0) {
          println(bestCache2.size)
        }
        bestCache2.update(key, state)
        val results = taskDefinition.successors2(state)
//        println(s"From $state => ")
//        state.debug(taskDefinition)
//        println(" potential following options exist =>")
//        results.foreach { x =>
//          x.debug(taskDefinition)
//        }
//        println
        results
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
    val realData = readFileText("2022/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    println(real.keySet)

    part1(test) shouldEqual 1651
    part1(real) shouldEqual 1701

    part2(test) shouldEqual 1707
    part2(real) shouldEqual 12345678
  }
}
