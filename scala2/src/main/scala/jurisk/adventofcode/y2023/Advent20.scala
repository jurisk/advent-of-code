package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.Advent20.Module.Broadcaster
import jurisk.adventofcode.y2023.Advent20.Module.Conjunction
import jurisk.adventofcode.y2023.Advent20.Module.FlipFlop
import jurisk.adventofcode.y2023.Advent20.Module.Output
import jurisk.adventofcode.y2023.Advent20.Pulse.High
import jurisk.adventofcode.y2023.Advent20.Pulse.Low
import jurisk.math.lcmMany
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Advent20 {
  type Input              = Map[ModuleName, Module]
  private type ModuleName = String
  private val BroadcasterName: ModuleName = "broadcaster"

  sealed trait Pulse
  object Pulse {
    case object Low  extends Pulse
    case object High extends Pulse
  }

  sealed trait Module extends Product with Serializable
  object Module {
    final case class Broadcaster(sendTo: List[ModuleName]) extends Module
    final case class FlipFlop(sendTo: List[ModuleName])    extends Module
    final case class Conjunction(sendTo: List[ModuleName]) extends Module
    case object Output                                     extends Module

    def parse(s: String): (ModuleName, Module) =
      s match {
        case s"broadcaster -> $list" =>
          BroadcasterName -> Broadcaster(list.commaSeparatedList)
        case s"%$name -> $list"      => name -> FlipFlop(list.commaSeparatedList)
        case s"&$name -> $list"      => name -> Conjunction(list.commaSeparatedList)
        case _                       => s.failedToParse
      }
  }

  private type ConjunctionState = Map[ModuleName, Pulse]

  final case class Message(from: ModuleName, to: ModuleName, pulse: Pulse)

  final case class State(
    messageQueue: Queue[Message],
    pulsesSent: Map[Pulse, Long],
    flipFlops: Map[ModuleName, Boolean],
    conjunctions: Map[ModuleName, ConjunctionState],
    buttonPresses: Long = 0,
    conjunctionsTriggered: Set[ModuleName],
  ) {
    private def send(qe: Message): State =
      copy(
        messageQueue = messageQueue.appended(qe),
        pulsesSent = pulsesSent + (qe.pulse -> (pulsesSent(qe.pulse) + 1)),
      )

    private def sendMany(
      from: ModuleName,
      to: List[ModuleName],
      pulse: Pulse,
    ): State =
      to.foldLeft(this) { case (acc, m) =>
        acc.send(Message(from, m, pulse))
      }

    private def process(module: Module, qe: Message): State = {
      val pulse = qe.pulse
      val from  = qe.from
      val to    = qe.to

      module match {
        case Module.Broadcaster(sendTo) =>
          sendMany(BroadcasterName, sendTo, pulse)
        case Module.FlipFlop(sendTo)    =>
          pulse match {
            case High =>
              this
            case Low  =>
              val onOff = flipFlops(to)
              copy(flipFlops = flipFlops + (to -> !onOff))
                .sendMany(to, sendTo, if (onOff) Low else High)
          }
        case Module.Conjunction(sendTo) =>
          val updatedIncomingWires = conjunctions(to) + (from -> pulse)
          val next                 =
            if (updatedIncomingWires.values.forall(_ == High)) Low else High
          val withUpdatedWires     =
            copy(conjunctions = conjunctions + (to -> updatedIncomingWires))
          val withUpdatedTriggered = if (next == Low) {
            withUpdatedWires.copy(
              conjunctionsTriggered = conjunctionsTriggered + to
            )
          } else withUpdatedWires

          withUpdatedTriggered.sendMany(to, sendTo, next)
        case Module.Output              =>
          this
      }
    }

    @tailrec
    private def runToCompletion(data: Input): State =
      messageQueue.headOption match {
        case Some(message) =>
          copy(messageQueue = messageQueue.tail)
            .process(data.getOrElse(message.to, Output), message)
            .runToCompletion(data)
        case None          => this
      }

    def next(data: Input): State =
      copy(buttonPresses = buttonPresses + 1)
        .send(Message("", BroadcasterName, Low))
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
        messageQueue = Queue.empty,
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

  def solve1(data: Input, times: Int): State =
    Simulation.runNIterations(State.initial(data), times) { case (state, _) =>
      state.next(data)
    }

  def part1(data: Input): Long = {
    val state = solve1(data, 1000)
    state.pulsesSent.values.product
  }

  private def runPatched(
    data: Input,
    seed: ModuleName,
    interest: ModuleName,
  ): Long = {
    val patched = data + (BroadcasterName -> Broadcaster(seed :: Nil))
    val initial = State.initial(patched)

    val result = Simulation.runWithIterationCount(initial) { case (state, _) =>
      val next = state.next(data)
      if (next.conjunctionsTriggered.contains(interest)) {
        next.asLeft
      } else {
        next.asRight
      }
    }

    result.buttonPresses
  }

  def part2(data: Input, pairs: List[(ModuleName, ModuleName)]): Long = {
    val results = pairs map { case (seed, input) =>
      runPatched(data, seed, input)
    }

    lcmMany(results)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/20$suffix.txt"

  // The solution is not universal - you have to look at the nodes in the input and hand-craft this input data.
  // The first ModuleNames are all of the ones that "broadcast" connects to.
  // The second ModuleNames are at a distance 3 conjunction nodes from "rx", e.g. "qq" for "qq -> ft -> xm -> rx".
  // The first and second ModuleNames in the tuple have to be reachable from each other.
  // Note that the test data input format is quite close to DOT used by GraphViz - see `20.dot` and analyse at
  // https://dreampuf.github.io/GraphvizOnline/.
  private[y2023] val SeedToInterestNodes: List[(ModuleName, ModuleName)] =
    ("pt", "qq") :: ("tp", "fj") :: ("bv", "jc") :: ("gv", "vm") :: Nil

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData, SeedToInterestNodes)}")
  }
}
