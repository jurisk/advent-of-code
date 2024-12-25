package jurisk.adventofcode.y2024

import cats.effect.IO
import cats.effect.IOApp
import jurisk.adventofcode.y2024.Advent24.Operation.And
import jurisk.adventofcode.y2024.Advent24.Operation.Or
import jurisk.adventofcode.y2024.Advent24.Operation.Xor
import jurisk.collections.immutable.SetOfTwo
import jurisk.utils.ConversionOps.BooleanOps
import jurisk.utils.ConversionOps.IntOps
import jurisk.utils.FileInput._
import jurisk.utils.FileInputIO
import jurisk.utils.Parsing.StringOps
import mouse.all.booleanSyntaxMouse

object Advent24 extends IOApp.Simple {
  private type Wire = String
  type Input        = (Map[Wire, Boolean], Connections)

  private def replace(s: Wire, replacements: Map[Wire, Wire]): Wire =
    replacements.getOrElse(s, s)

  final case class Connections(set: Set[Connection]) {
    def allWires: Set[Wire]                           = set.flatMap(_.wiresMentioned)
    def foreach(f: Connection => Unit): Unit          = set foreach f
    def map(f: Connection => Connection): Connections = copy(set = set map f)
    def -(c: Connection): Connections                 = copy(set = set - c)

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

    def fix: (Connections, List[SetOfTwo[Wire]]) = {
      // TODO: pick random (or non-random, use bits you know are bad) swap, try it, if it is better than before, continue with that, if it is 0 errors then return, if it is worse, continue with this

      // These were found by reviewing the generated DOT file
      val swaps = List(
        SetOfTwo("hbk", "z14"),
        SetOfTwo("kvn", "z18"),
        SetOfTwo("dbb", "z23"),
        SetOfTwo("cvh", "tfn"),
      )

      val swapped = swaps.foldLeft(set) { case (acc, swap) =>
        acc.map(_.swapOutput(swap))
      }

      val result = Connections(swapped)
      val errors = errorsOnAddition(result)
      assert(errors == 0)

      (result, swaps)
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
      s => Connections(s.splitLines.toSet map Connection.parse),
    )

  private def propagate(
    wires: Map[Wire, Boolean],
    connections: Connections,
  ): Map[Wire, Boolean] = {
    // TODO: Would https://en.wikipedia.org/wiki/Topological_sorting be more efficient?
    var results = wires
    var queue   = connections.allWires
    var useful  = true
    while (useful) {
      useful = false
      connections foreach { c =>
        if (
          results.contains(c.a) && results
            .contains(c.b) && !results.contains(c.out)
        ) {
          results += (c.out -> c.result(results))
          queue -= c.out
          useful = true
        }
      }
    }
    results
  }

  def part1(data: Input): BigInt = {
    val (wires, connections) = data
    val results              = propagate(wires, connections)
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

  private val InputBits  = 45
  private val OutputBits = InputBits + 1

  private def errorsOnAddition(connections: Connections): Int = {
    def errorsAddingBit(bit: Int, connections: Connections): Int = {
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
        val values    = zeroWires ++ Map(xReg(bit) -> x, yReg(bit) -> y)
        val output    = propagate(values, connections)
        val invalidR  = output.getOrElse(zReg(bit), false) != r
        val carryBit  = bit + 1
        val invalidC  = output.getOrElse(zReg(carryBit), false) != c
        val extraBits = (0 until OutputBits)
          .filter { b =>
            b != bit && b != carryBit
          }
          .count { i =>
            output.getOrElse(zReg(i), false)
          }
        if (invalidR || invalidC || extraBits > 0) {
          println(s"bit: $bit, x: $x, y: $y, r: $r")
          println(s"output: $output")
        }
        invalidR.toInt + invalidC.toInt + extraBits
      }.sum
    }

    (0 until InputBits).map { bit =>
      errorsAddingBit(bit, connections)
    }.sum
  }

  def part2(data: Input): String = {
    val (_, connections) = data

    val (_, swaps) = connections.fix

    swaps
      .flatMap { swap =>
        swap.toSet
      }
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
