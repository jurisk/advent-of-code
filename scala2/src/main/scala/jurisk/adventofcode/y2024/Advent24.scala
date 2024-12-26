package jurisk.adventofcode.y2024

import cats.effect.IO
import cats.effect.IOApp
import jurisk.adventofcode.y2024.Advent24.Operation.And
import jurisk.adventofcode.y2024.Advent24.Operation.Or
import jurisk.adventofcode.y2024.Advent24.Operation.Xor
import jurisk.algorithms.graph.GraphAlgorithms
import jurisk.collections.immutable.SetOfTwo
import jurisk.math.LongOps
import jurisk.utils.CollectionOps.OptionOps
import jurisk.utils.ConversionOps.BooleanOps
import jurisk.utils.ConversionOps.IntOps
import jurisk.utils.FileInput._
import jurisk.utils.FileInputIO
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

import scala.annotation.tailrec
import scala.util.Random

// Notes:
// - I actually solved this by simplifying the output DOT file and then finding irregularities manually.
// - Later, I tried to apply a genetic algorithm, but failed to get this to converge.
object Advent24 extends IOApp.Simple {
  private val InputBits  = 45
  private val OutputBits = InputBits + 1

  type Input = (Values, Connections)

  final case class Values private (map: Map[Wire, Boolean]) {
    def getOrFalse(wire: Wire): Boolean  = map.getOrElse(wire, false)
    def contains(wire: Wire): Boolean    = map.contains(wire)
    def +(pair: (Wire, Boolean)): Values = new Values(map + pair)
    def ++(other: Values): Values        = new Values(map ++ other.map)

    private def bitsToLong(bits: Map[Int, Boolean]) = {
      val bitsStr = bits.toSeq.sorted
        .map { case (_, b) => if (b) "1" else "0" }
        .mkString
        .reverse
      java.lang.Long.parseLong(bitsStr, 2)
    }

    def xValue: Long = {
      val z = map.collect { case (Wire.X(zIdx), v) => (zIdx, v) }
      bitsToLong(z)
    }

    def yValue: Long = {
      val z = map.collect { case (Wire.Y(zIdx), v) => (zIdx, v) }
      bitsToLong(z)
    }

    def zValue: Long = {
      val z = map.collect { case (Wire.Z(zIdx), v) => (zIdx, v) }
      bitsToLong(z)
    }
  }

  private object Values {
    val Zero: Values = Values {
      (0 until InputBits).flatMap { b =>
        List(
          Wire.X(b) -> false,
          Wire.Y(b) -> false,
        )
      }.toMap
    }

    def randomXY: Values = Values {
      (0 until InputBits).flatMap { b =>
        List(
          Wire.X(b) -> Random.nextBoolean(),
          Wire.Y(b) -> Random.nextBoolean(),
        )
      }.toMap
    }

    def apply(pairs: (Wire, Boolean)*): Values = Values(pairs.toMap)

    def parse(s: String): Values = {
      val map = s.splitLines
        .map(
          _.parsePairUnsafe(": ", Wire.parse, _.toInt.toBooleanStrict01Unsafe)
        )
        .toMap
      new Values(map)
    }
  }

  sealed trait Wire extends Product with Serializable
  private object Wire {
    final case class X(i: Int)                         extends Wire {
      override def toString: String = f"x$i%02d"
    }
    final case class Y(i: Int)                         extends Wire {
      override def toString: String = f"y$i%02d"
    }
    final case class Z(i: Int)                         extends Wire {
      override def toString: String = f"z$i%02d"
    }
    final case class Middle(a: Char, b: Char, c: Char) extends Wire {
      override def toString: String = s"$a$b$c"
    }

    def parse(s: String): Wire = s match {
      case s"x$i" => X(i.toInt)
      case s"y$i" => Y(i.toInt)
      case s"z$i" => Z(i.toInt)
      case s      =>
        s.toList match {
          case List(a, b, c) => Middle(a, b, c)
          case _             => s"Failed to parse $s".fail
        }
    }
  }

  object Connections {
    def parse(s: String): Connections =
      Connections.fromIterable(s.splitLines.toSet map Connection.parse)

    private def fromIterable(set: Iterable[(Connection, Wire)]): Connections =
      new Connections(
        set
          .groupBy { case (_, out) => out }
          .view
          .mapValues { connections =>
            assert(connections.size == 1)
            val (connection, _) = connections.head
            connection
          }
          .toMap
      )
  }

  final case class Connections private (map: Map[Wire, Connection]) {
    val allWires: Set[Wire]           = map.flatMap { case (k, v) =>
      Set(k, v.a, v.b)
    }.toSet
    private val allOutputs: Set[Wire] = map.keySet

    def foreach(f: Connection => Unit): Unit = map.values foreach f

    private def errorsOnAddition: Option[Int] =
      // We care more about `errorsBitByBit`, but since they didn't catch everything, we also care about `errorsOnRandomAddition`
      isValid.option(128 * errorsBitByBit + errorsOnRandomAddition)

    private def errorsOnRandomAddition: Int = {
      val Samples = 16
      (for {
        _             <- 0 until Samples
        r              = Values.randomXY
        x              = r.xValue
        y              = r.yValue
        expectedZ      = x + y
        solved         = propagate(r).orFail("Failed to propagate")
        z              = solved.zValue
        wrongBits      = z ^ expectedZ
        wrongBitsCount = wrongBits.bitCount
      } yield wrongBitsCount).sum
    }

    private def errorsBitByBit: Int = {
      def errorsAddingBit(bit: Int): Int =
        List(
          (false, false, false, false),
          (false, true, true, false),
          (true, false, true, false),
          (true, true, false, true),
        ).map { case (x, y, r, c) =>
          val values     = Values.Zero ++ Values(Wire.X(bit) -> x, Wire.Y(bit) -> y)
          val output     = propagate(values).orFail("Failed to propagate")
          val invalidR   = output.getOrFalse(Wire.Z(bit)) != r
          val carryBit   = bit + 1
          val invalidC   = output.getOrFalse(Wire.Z(carryBit)) != c
          val extraBits  = (0 until OutputBits)
            .filter { b =>
              b != bit && b != carryBit
            }
            .count { i =>
              output.getOrFalse(Wire.Z(i))
            }
          val DebugPrint = false
          if (DebugPrint && (invalidR || invalidC || extraBits > 0)) {
            println(s"bit: $bit, x: $x, y: $y, r: $r")
            println(s"output: $output")
          }
          invalidR.toInt + invalidC.toInt + extraBits
        }.sum

      (0 until InputBits).map { bit =>
        errorsAddingBit(bit)
      }.sum
    }

    private def isValid: Boolean = topologicallySortedWires.isDefined

    private val topologicallySortedWires: Option[List[Wire]] = {
      val edges = map.toSeq.flatMap { case (out, c) =>
        Set(c.a -> out, c.b -> out)
      }

      val graph = GraphAlgorithms.createAdjacencyMapDirected(edges)
      GraphAlgorithms.topologicalSort(graph)
    }

    def propagate(
      wires: Values
    ): Option[Values] =
      topologicallySortedWires map { sorted =>
        sorted.foldLeft(wires) { case (values, wire) =>
          if (values.contains(wire)) {
            values
          } else {
            val connection = map(wire)
            values + (wire -> connection.result(values))
          }
        }
      }

    private def swapOutputs(swap: SetOfTwo[Wire]): Connections = {
      val (a, b) = swap.tupleInArbitraryOrder
      val aC     = map(a)
      val bC     = map(b)
      val newMap = map ++ Map(
        a -> bC,
        b -> aC,
      )
      new Connections(newMap)
    }

    def fix: (Connections, Set[SetOfTwo[Wire]]) = {
      @tailrec
      def f(
        current: Connections,
        currentScore: Int,
        currentSwaps: Set[SetOfTwo[Wire]],
      ): (Connections, Set[SetOfTwo[Wire]]) = {
        println(s"Current score: $currentScore, Current swaps: $currentSwaps")
        if (currentScore == 0) {
          (current, currentSwaps)
        } else {
          // Note: The swaps for our data are:
          // 1. hbk <-> z14
          // 2. kvn <-> z18
          // 3. dbb <-> z23
          // 4. cvh <-> tfn
          val candidates = current.allOutputs.toIndexedSeq
          (for {
            aIdx   <- candidates.indices
            bIdx   <- candidates.indices
            if aIdx < bIdx
            a       = candidates(aIdx)
            b       = candidates(bIdx)
            swap    = SetOfTwo(a, b)
            swapped = current.swapOutputs(swap)
            if swapped.isValid
          } yield (swap, swapped))
            .map { case (swap, c) =>
              (c, c.errorsOnAddition.orFail("Failed to get errors"), swap)
            }
            .minBy { case (_, score, _) => score } match {
            case (c, score, swap) if score < currentScore =>
              f(c, score, currentSwaps + swap)
            case _                                        =>
              println("No more improvements")
              (current, currentSwaps)
          }
        }
      }

      f(this, errorsOnAddition.orFail("Failed"), Set.empty)
    }
  }

  final case class Connection(a: Wire, b: Wire, op: Operation) {
    def result(values: Values): Boolean = {
      val aV = values.getOrFalse(a)
      val bV = values.getOrFalse(b)
      op match {
        case And => aV && bV
        case Or  => aV || bV
        case Xor => aV ^ bV
      }
    }

    def name: String = s"$a ${op.name} $b"
  }

  sealed trait Operation extends Product with Serializable {
    def name: String = this match {
      case And => "AND"
      case Or  => "OR"
      case Xor => "XOR"
    }
  }
  object Operation {
    case object And extends Operation
    case object Or  extends Operation
    case object Xor extends Operation
  }

  private object Connection {
    private val RegEx                        = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r
    def parse(s: String): (Connection, Wire) =
      s match {
        case RegEx(a, op, b, out) =>
          val operation: Operation = op match {
            case "AND" => And
            case "OR"  => Or
            case "XOR" => Xor
            case _     => s.failedToParse
          }

          // All these operations are commutative, so we can sort the inputs to gain more symmetries
          val lowest  = List(a, b).min
          val highest = List(a, b).max

          val connection = Connection(
            Wire.parse(lowest),
            Wire.parse(highest),
            operation,
          )
          val output     = Wire.parse(out)
          (connection, output)
        case _                    => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parsePairByDoubleNewline(
      Values.parse,
      Connections.parse,
    )

  def part1(data: Input): Long = {
    val (wires, connections) = data
    val results              = connections.propagate(wires).orFail("Failed to propagate")
    results.zValue
  }

  private def debugWrite(connections: Connections): IO[Unit] = {
    val nodeStrings = List(("x", "blue"), ("y", "green"), ("z", "red"))
      .map { case (prefix, colour) =>
        connections.allWires
          .filter(_.toString.startsWith(prefix))
          .map(w => s"""  $w [shape=box, color=$colour];""")
          .mkString("\n")
      }
      .mkString("\n\n")

    val ops = connections.map map { case (out, connection) =>
      val opName = s""""${connection.name}""""
      s"""
         |  ${connection.a} -> $opName
         |  ${connection.b} -> $opName
         |  $opName -> $out
         |
         |""".stripMargin
    }

    val output = s"""digraph G {
                    |$nodeStrings
                    |
                    |${ops.mkString}
                    |}
                    |""".stripMargin

    FileInputIO.writeFileText("temp.dot", output)
  }

  def part2(data: Input): String = {
    val (_, connections) = data

    val (_, swaps) = connections.fix

    swaps
      .flatMap(_.toSet)
      .toList
      .map(_.toString)
      .sorted
      .mkString(",")
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/24$suffix.txt"

  private val DebugWrite     = false
  override def run: IO[Unit] = for {
    (wires, connections) <- IO(parseFile(fileName("")))
    _                    <- DebugWrite.whenA(debugWrite(connections))
    _                    <- IO.println(s"Part 1: ${part1((wires, connections))}")
    _                    <- IO.println(s"Part 2: ${part2((wires, connections))}")
  } yield ()
}
