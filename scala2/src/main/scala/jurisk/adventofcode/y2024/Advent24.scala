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
  private type Wire = String
  type Input        = (Map[Wire, Boolean], Connections)

  private def replace(s: Wire, replacements: Map[Wire, Wire]): Wire =
    replacements.getOrElse(s, s)

  object Connections {
    private val InputBits  = 45
    private val OutputBits = InputBits + 1

    def parse(s: String): Connections =
      Connections(s.splitLines.toSet map Connection.parse)
  }

  // TODO: Make it a Map[Wire, (Wire, Operation, Wire)] as you are filtering by `out` a lot
  final case class Connections(set: Set[Connection]) {
    val outputWires: Set[Wire] = set.map(_.out)
    val allWires: Set[Wire]    = set.flatMap(_.wiresMentioned)

    def foreach(f: Connection => Unit): Unit          = set foreach f
    def map(f: Connection => Connection): Connections = copy(set = set map f)
    def -(c: Connection): Connections                 = copy(set = set - c)

    private def errorsOnAddition: Int = {
      def errorsAddingBit(bit: Int): Int = {
        def zeroWires: Map[Wire, Boolean] =
          (0 until InputBits).flatMap { b =>
            List(
              xReg(b) -> false,
              yReg(b) -> false,
            )
          }.toMap

        List(
          (false, false, false, false),
          (false, true, true, false),
          (true, false, true, false),
          (true, true, false, true),
        ).map { case (x, y, r, c) =>
          val values     = zeroWires ++ Map(xReg(bit) -> x, yReg(bit) -> y)
          val output     = propagate(values).orFail("Failed to propagate")
          val invalidR   = output.getOrElse(zReg(bit), false) != r
          val carryBit   = bit + 1
          val invalidC   = output.getOrElse(zReg(carryBit), false) != c
          val extraBits  = (0 until OutputBits)
            .filter { b =>
              b != bit && b != carryBit
            }
            .count { i =>
              output.getOrElse(zReg(i), false)
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
      val edges = set.flatMap { c =>
        Set(c.a -> c.out, c.b -> c.out)
      }

      val graph = GraphAlgorithms.createAdjacencyMapDirected(edges.toSeq)
      GraphAlgorithms.topologicalSort(graph)
    }

    def propagate(
      wires: Map[Wire, Boolean]
    ): Option[Map[Wire, Boolean]] =
      topologicallySortedWires map { sorted =>
        sorted.foldLeft(wires) { case (values, wire) =>
          if (values.contains(wire)) {
            values
          } else {
            val connections = set.filter(_.out == wire)
            val newValues   = connections.foldLeft(values) {
              case (values, connection) =>
                values + (connection.out -> connection.result(values))
            }
            newValues
          }
        }
      }

    def simplify: Connections = {
      def rename(
        connections: Set[Connection],
        what: Wire,
        toWhat: Wire,
      ): Set[Connection] =
        connections.map(_.rename(what, toWhat))

      def simplifyBit(
        connections: Set[Connection],
        bit: Int,
      ): Set[Connection] = {

        def simplifyOp(
          operation: Operation,
          connections: Set[Connection],
        ): Set[Connection] =
          connections.find {
            case Connection(a, b, `operation`, _) =>
              Set(a, b) == Set(xReg(bit), yReg(bit))
            case _                                =>
              false
          } match {
            case Some(a @ Connection(_, _, `operation`, out)) =>
              rename(connections - a, out, f"${operation.name}_$bit%02d_$out")
            case _                                            =>
              println(s"Bit $bit: $operation not found")
              connections
          }

        List(Xor, And).foldLeft(connections) { case (acc, op) =>
          simplifyOp(op, acc)
        }
      }

      Connections {
        (0 until InputBits).foldLeft(set) { case (ops, bit) =>
          simplifyBit(ops, bit)
        }
      }
    }

    private def swapOutputs(swap: SetOfTwo[Wire]): Connections =
      map(_.swapOutput(swap))

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
          // TODO: Have a wider set of swaps to pick from!
          val swaps      = Set(
            SetOfTwo("hbk", "z14"),
            SetOfTwo("kvn", "z18"),
            SetOfTwo("dbb", "z23"),
            SetOfTwo("cvh", "tfn"),
            SetOfTwo("z13", "z12"), // This one just to mess things up
          )
          val candidates = swaps.flatMap(_.toSet)
          (for {
            a      <- candidates
            b      <- candidates
            if a < b
            swap    = SetOfTwo(a, b)
            swapped = current.swapOutputs(swap)
            if swapped.isValid
          } yield (swap, swapped))
            .map { case (swap, c) => (c, c.errorsOnAddition, swap) }
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

  final case class Connection(a: Wire, b: Wire, op: Operation, out: Wire) {
    def wiresMentioned: Set[Wire] = Set(a, b, out)

    def result(values: Map[Wire, Boolean]): Boolean = {
      val aV = values.getOrElse(a, false)
      val bV = values.getOrElse(b, false)
      op match {
        case And => aV && bV
        case Or  => aV || bV
        case Xor => aV ^ bV
      }
    }

    def rename(what: Wire, toWhat: Wire): Connection = {
      val m = Map(what -> toWhat)
      copy(a = replace(a, m), b = replace(b, m), out = replace(out, m))
    }

    def swapOutput(swap: SetOfTwo[Wire]): Connection = {
      val (a, b) = swap.tupleInArbitraryOrder
      val m      = Map(a -> b, b -> a)
      copy(out = replace(out, m))
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
    private val RegEx                = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r
    def parse(s: String): Connection =
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

          Connection(lowest, highest, operation, out)
        case _                    => s.failedToParse
      }
  }

  def parse(input: String): Input =
    input.parsePairByDoubleNewline(
      _.splitLines
        .map(
          _.parsePairUnsafe(": ", identity, _.toInt.toBooleanStrict01Unsafe)
        )
        .toMap,
      Connections.parse,
    )

  def part1(data: Input): BigInt = {
    val (wires, connections) = data
    val results              = connections.propagate(wires).orFail("Failed to propagate")
    val z                    = results.filter { case (k, _) => k.startsWith("z") }.toList
    val zBits                =
      z.sorted.map { case (_, b) => if (b) "1" else "0" }.mkString.reverse
    BigInt(zBits, 2)
  }

  private def debugWrite(connections: Connections): IO[Unit] = {
    val nodeStrings = List(("x", "blue"), ("y", "green"), ("z", "red"))
      .map { case (prefix, colour) =>
        connections.allWires
          .filter(_.startsWith(prefix))
          .map(w => s"""  $w [shape=box, color=$colour];""")
          .mkString("\n")
      }
      .mkString("\n\n")

    val ops = connections.set map { connection =>
      val opName = s""""${connection.name}""""
      s"""
         |  ${connection.a} -> $opName
         |  ${connection.b} -> $opName
         |  $opName -> ${connection.out}
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

  private def xReg(i: Int): Wire = f"x$i%02d"
  private def yReg(i: Int): Wire = f"y$i%02d"
  private def zReg(i: Int): Wire = f"z$i%02d"

  def part2(data: Input): String = {
    val (_, connections) = data

    val (_, swaps) = connections.fix

    swaps
      .flatMap(_.toSet)
      .toList
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
    _                    <- DebugWrite.whenA {
                              val (fixed, _) = connections.fix
                              val simplified = fixed.simplify
                              debugWrite(simplified)
                            }
    _                    <- IO.println(s"Part 1: ${part1((wires, connections))}")
    _                    <- IO.println(s"Part 2: ${part2((wires, connections))}")
  } yield ()
}
