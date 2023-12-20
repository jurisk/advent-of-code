package jurisk.adventofcode.y2023

import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId}
import jurisk.adventofcode.y2023.Advent20.Module.{
  Broadcaster,
  Conjunction,
  FlipFlop,
  Output,
}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Advent20 {
  type Input      = Map[ModuleName, Module]
  type ModuleName = String
  val Start: ModuleName = "broadcaster"

  type Pulse = Boolean
  val Low: Pulse  = false
  val High: Pulse = true

  sealed trait Module extends Product with Serializable
  object Module {
    final case class Broadcaster(sendTo: List[ModuleName]) extends Module
    final case class FlipFlop(sendTo: List[ModuleName])    extends Module
    final case class Conjunction(sendTo: List[ModuleName]) extends Module
    final case object Output                               extends Module

    def parse(s: String): (ModuleName, Module) =
      s match {
        case s"broadcaster -> $list" =>
          "broadcaster" -> Broadcaster(list.split(", ").toList)
        case s"%$name -> $list"      => name -> FlipFlop(list.split(", ").toList)
        case s"&$name -> $list"      => name -> Conjunction(list.split(", ").toList)
        case _                       => s.failedToParse
      }
  }

  type ConjunctionState = Map[ModuleName, Pulse]

  final case class QueueEntry(from: ModuleName, to: ModuleName, pulse: Pulse)

  final case class State(
    pulsesQueue: Queue[QueueEntry],
    pulsesSent: Map[Pulse, Long],
    flipFlops: Map[ModuleName, Boolean],
    conjunctions: Map[ModuleName, ConjunctionState],
    buttonPresses: Long = 0,
    conjunctionsTriggered: Set[ModuleName],
  ) {
    override def toString: String =
      s"FF: ${flipFlops.filter(_._2).keys.toList.sorted.mkString(", ")}\nCJ: ${conjunctions
          .map { case (k, v) =>
            s"$k has on [${v.filter(_._2).keys.toList.sorted.mkString(", ")}]"
          }}\n\n"

    private def send(qe: QueueEntry): State =
      copy(
        pulsesQueue = pulsesQueue.appended(qe),
        pulsesSent = pulsesSent + (qe.pulse -> (pulsesSent(qe.pulse) + 1)),
      )

    private def process(module: Module, qe: QueueEntry): State = {
      val pulse = qe.pulse
      val from  = qe.from
      val to    = qe.to

      module match {
        case Module.Broadcaster(sendTo) =>
          sendTo.foldLeft(this) { case (acc, m) =>
            acc.send(QueueEntry(Start, m, pulse))
          }
        case Module.FlipFlop(sendTo)    =>
          (pulse, flipFlops(to)) match {
            case (`High`, _) => this
            case (`Low`, b)  =>
              val q = copy(flipFlops = flipFlops + (to -> !b))
              sendTo.foldLeft(q) { case (acc, m) =>
                acc.send(QueueEntry(to, m, !b))
              }
            case (a, b)      => s"$a $b".fail // TODO
          }
        case Module.Conjunction(sendTo) =>
          val oldCs     = conjunctions(to)
          val updatedCs = oldCs + (from -> pulse)
          val next      = if (updatedCs.values.forall(_ == High)) Low else High
          val b         = copy(conjunctions = conjunctions + (to -> updatedCs))
          val updated   = if (next == Low) {
            b.copy(
              conjunctionsTriggered = conjunctionsTriggered + to
            )
          } else b

          sendTo.foldLeft(updated) { case (acc, m) =>
            acc.send(QueueEntry(to, m, next))
          }
        case Module.Output              =>
          this
      }
    }

    @tailrec
    private def runToCompletion(data: Input): State =
      pulsesQueue.headOption match {
        case Some(e) =>
          val result = copy(pulsesQueue = pulsesQueue.tail)
            .process(data.getOrElse(e.to, Output), e)
          result.runToCompletion(data)
        case None    => this
      }

    def next(data: Input): State =
      copy(buttonPresses = buttonPresses + 1)
        .send(QueueEntry("", Start, Low))
        .runToCompletion(data)
  }

  object State {
    def initial(data: Input): State = {
      def whoSendsTo(n: ModuleName): List[ModuleName] =
        data
          .filter { case (_, v) =>
            v match {
              case Module.Broadcaster(sendTo) => sendTo.contains(n)
              case FlipFlop(sendTo)           => sendTo.contains(n)
              case Conjunction(sendTo)        => sendTo.contains(n)
              case Output                     => false
            }
          }
          .keys
          .toList

      State(
        pulsesQueue = Queue.empty,
        pulsesSent = Map(Low -> 0, High -> 0),
        flipFlops = data.collect { case (s, _: FlipFlop) =>
          s -> false
        },
        conjunctions = data.collect { case (s, _: Conjunction) =>
          s -> whoSendsTo(s).map(_ -> Low).toMap
        },
        conjunctionsTriggered = Set.empty,
      )
    }
  }

  def parse(input: String): Input =
    input.parseLines(Module.parse).toMap

  def solve1(data: Input, times: Int): State = {
    data foreach println

    Simulation.runNIterations(State.initial(data), times) {
      case (state, counter) =>
        println(s"$counter $state")
        state.next(data)
    }
  }

  def part1(data: Input): Long = {
    val state = solve1(data, 1000)
    state.pulsesSent.values.product
  }

  private def debugMap(map: Map[ModuleName, Boolean]): String =
    map.filter(_._2).keys.toList.sorted.mkString(", ")

  private def conjunctionsOfInterest(
    state: State,
    interest: List[ModuleName],
  ): String =
    state.conjunctions
      .filter { case (k, v) =>
        interest.contains(k) && v.values.forall(_ == true)
      }
      .keys
      .toList
      .sorted
      .mkString(", ")

  def runPatched(data: Input, seed: ModuleName, interest: ModuleName): Long = {
    val patched = data + (Start -> Broadcaster(seed :: Nil))
    val initial = State.initial(patched)

    val result = Simulation.runWithIterationCount(initial) { case (state, _) =>
      val res = state.next(data)
      if (state.conjunctionsTriggered.contains(interest)) {
        state.asLeft
      } else {
        res.asRight
      }
    }

    result.buttonPresses
  }

  def part2(data: Input): Long = {
    val n1 = runPatched(data, "pt", "qq")
    val n2 = runPatched(data, "tp", "fj")
    val n3 = runPatched(data, "bv", "jc")
    val n4 = runPatched(data, "gv", "vm")

    n1 * n2 * n3 * n4
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/20$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
