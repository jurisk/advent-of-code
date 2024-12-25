package jurisk.adventofcode.y2024

import cats.effect.IO
import cats.effect.IOApp
import jurisk.adventofcode.y2024.Advent24.Connections.InputBits
import jurisk.adventofcode.y2024.Advent24.Connections.OutputBits
import jurisk.adventofcode.y2024.Advent24.Operation.And
import jurisk.adventofcode.y2024.Advent24.Operation.Or
import jurisk.adventofcode.y2024.Advent24.Operation.Xor
import jurisk.algorithms.graph.GraphAlgorithms
import jurisk.collections.immutable.SetOfTwo
import jurisk.utils.CollectionOps.OptionOps
import jurisk.utils.ConversionOps.BooleanOps
import jurisk.utils.ConversionOps.IntOps
import jurisk.utils.FileInput._
import jurisk.utils.FileInputIO
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

import scala.annotation.tailrec

object Advent24 extends IOApp.Simple {
  private type Values = Map[Wire, Boolean]
  type Input          = (Values, Connections)

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

  private def replace(s: Wire, replacements: Map[Wire, Wire]): Wire =
    replacements.getOrElse(s, s)

  object Connections {
    private val InputBits  = 45
    private val OutputBits = InputBits + 1

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
    val allWires: Set[Wire]   = map.flatMap { case (k, v) =>
      Set(k, v.a, v.b)
    }.toSet
    val allOutputs: Set[Wire] = map.keySet

    def foreach(f: Connection => Unit): Unit = map.values foreach f

    // TODO: This doesn't do a sufficient test, as these bit-by-bit tests don't catch all issues that could happen. Consider adding random numbers.
    private def errorsOnAddition: Int = {
      def errorsAddingBit(bit: Int): Int = {
        def zeroWires: Values =
          (0 until InputBits).flatMap { b =>
            List(
              Wire.X(b) -> false,
              Wire.Y(b) -> false,
            )
          }.toMap

        List(
          (false, false, false, false),
          (false, true, true, false),
          (true, false, true, false),
          (true, true, false, true),
        ).map { case (x, y, r, c) =>
          val values     = zeroWires ++ Map(Wire.X(bit) -> x, Wire.Y(bit) -> y)
          val output     = propagate(values).orFail("Failed to propagate")
          val invalidR   = output.getOrElse(Wire.Z(bit), false) != r
          val carryBit   = bit + 1
          val invalidC   = output.getOrElse(Wire.Z(carryBit), false) != c
          val extraBits  = (0 until OutputBits)
            .filter { b =>
              b != bit && b != carryBit
            }
            .count { i =>
              output.getOrElse(Wire.Z(i), false)
            }
          val DebugPrint = false
          if (DebugPrint && (invalidR || invalidC || extraBits > 0)) {
            println(s"bit: $bit, x: $x, y: $y, r: $r")
            println(s"output: $output")
          }
          invalidR.toInt + invalidC.toInt + extraBits
        }.sum
      }

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
          // TODO: Try to apply Genetic Algorithm or similar...
          // TODO: Have a wider set of swaps to pick from!
          val swaps      = Set(
            SetOfTwo("hbk", "z14"),
            SetOfTwo("kvn", "z18"),
            SetOfTwo("dbb", "z23"),
            SetOfTwo("cvh", "tfn"),
          )
          val candidates = swaps.flatMap(_.toSet).map(Wire.parse).toIndexedSeq
//          val candidates = current.allOutputs
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
              (c, c.errorsOnAddition, swap)
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

      f(this, errorsOnAddition, Set.empty)
    }
  }

  final case class Connection(a: Wire, b: Wire, op: Operation) {
    def result(values: Values): Boolean = {
      val aV = values.getOrElse(a, false)
      val bV = values.getOrElse(b, false)
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
      _.splitLines
        .map(
          _.parsePairUnsafe(": ", Wire.parse, _.toInt.toBooleanStrict01Unsafe)
        )
        .toMap,
      Connections.parse,
    )

  def part1(data: Input): BigInt = {
    val (wires, connections) = data
    val results              = connections.propagate(wires).orFail("Failed to propagate")
    val z                    = results.collect { case (Wire.Z(zIdx), v) => (zIdx, v) }.toList
    val zBits                =
      z.sorted.map { case (_, b) => if (b) "1" else "0" }.mkString.reverse
    BigInt(zBits, 2)
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
