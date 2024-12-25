package jurisk.adventofcode.y2024

import jurisk.adventofcode.y2024.Advent24.Operation.And
import jurisk.adventofcode.y2024.Advent24.Operation.Or
import jurisk.adventofcode.y2024.Advent24.Operation.Xor
import jurisk.utils.ConversionOps.IntOps
import jurisk.utils.FileInput._
import jurisk.utils.FileInputIO
import jurisk.utils.Parsing.StringOps

object Advent24 {
  private type Wire = String
  type Input        = (Map[Wire, Boolean], Set[Connection])

  type N = Long

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

    def rename(what: Wire, toWhat: Wire): Connection =
      if (a == what) {
        copy(a = toWhat)
      } else if (b == what) {
        copy(b = toWhat)
      } else if (out == what) {
        copy(out = toWhat)
      } else {
        this
      }

    def swapOutput(what: Wire, toWhat: Wire): Connection =
      if (out == what) {
        copy(out = toWhat)
      } else if (out == toWhat) {
        copy(out = what)
      } else {
        this
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
          val lowest  = List(a, b).min
          val highest = List(a, b).max

          val operation: Operation = op match {
            case "AND" => And
            case "OR"  => Or
            case "XOR" => Xor
            case _     => s.failedToParse
          }

          // All these operations are commutative, so we can sort the inputs to gain more symmetries
          Connection(lowest, highest, operation, out)
        case _                    => s.failedToParse
      }
  }

  def parse(input: String): Input = {
    val (wiresS, connections) = input.splitPairByDoubleNewline
    val wires                 = wiresS.splitLines map { w: String =>
      val (q, bbg) = w.splitPairUnsafe(": ")
      val i        = bbg.toInt
      val b        = i.toBooleanStrict01Unsafe
      (q, b)
    }
    val ops                   = connections.splitLines.toSet map Connection.parse
    (wires.toMap, ops)
  }

  private def propagate(
    wires: Map[Wire, Boolean],
    connections: Set[Connection],
  ): Map[Wire, Boolean] = {
    // TODO: Would https://en.wikipedia.org/wiki/Topological_sorting be more efficient?
    var results = wires
    var queue   = connections.flatMap(_.wiresMentioned)
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
    val sor                  =
      z.sorted.map { case (_, b) => if (b) "1" else "0" }.mkString.reverse
    BigInt(sor, 2)
  }

  private def debugWrite(connections: Set[Connection]): Unit = {
    val allWires = connections.flatMap { q: Connection => q.wiresMentioned }

    val nodeStrings = List(("x", "blue"), ("y", "green"), ("z", "red"))
      .map { case (prefix, colour) =>
        allWires
          .filter(_.startsWith(prefix))
          .map(w => s"""  $w [shape=box, color=$colour];""")
          .mkString("\n")
      }
      .mkString("\n\n")

    val ops = connections map { connection =>
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

  def xReg(i: Int): Wire = f"x$i%02d"
  def yReg(i: Int): Wire = f"y$i%02d"
  def zReg(i: Int): Wire = f"z$i%02d"

  private def zeroWires: Map[Wire, Boolean] =
    (0 until InputBits).flatMap { b =>
      List(
        xReg(b) -> false,
        yReg(b) -> false,
      )
    }.toMap

  // TODO: Calculate this metric for all bits
  private def testAddition(bit: Int, connections: Set[Connection]): Unit =
    List(
      (false, false, false, false),
      (false, true, true, false),
      (true, false, true, false),
      (true, true, false, true),
    ) foreach { case (x, y, r, c) =>
      val m         = zeroWires ++ Map(xReg(bit) -> x, yReg(bit) -> y)
      val o         = propagate(m, connections)
      val validR    = o.getOrElse(zReg(bit), false) == r
      val carryBit  = bit + 1
      val validC    = o.getOrElse(zReg(carryBit), false) == c
      val extraBits = (0 until OutputBits)
        .filter { b =>
          b != bit && b != carryBit
        }
        .count { i =>
          o.getOrElse(zReg(i), false)
        }
      if (!validR || !validC || extraBits > 0) {
        println(s"bit: $bit, x: $x, y: $y, r: $r")
        println(s"o: $o")
      }
    }

  private val InputBits  = 45
  private val OutputBits = InputBits + 1

  private def simplifyBit(
    connections: Set[Connection],
    bit: Int,
  ): Set[Connection] = {
    def rename(
      ops: Set[Connection],
      what: Wire,
      toWhat: Wire,
    ): Set[Connection] = {
      println(s"Renaming $what to $toWhat")
      ops.map(_.rename(what, toWhat))
    }

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

  def simplify(connections: Set[Connection]): Set[Connection] =
    (0 until InputBits).foldLeft(connections) { case (ops, bit) =>
      simplifyBit(ops, bit)
    }

  def part2(data: Input): String = {
    val (_, connections) = data

    // TODO: Find these automatically, by checking if the error count is lower if you swap nearby outputs
    // These were found by reviewing the generated DOT file
    val swaps =
      List(("hbk", "z14"), ("kvn", "z18"), ("dbb", "z23"), ("cvh", "tfn"))

    val swapped = swaps.foldLeft(connections) { case (acc, (a, b)) =>
      acc.map(_.swapOutput(a, b))
    }

    val simplified = simplify(swapped)
    debugWrite(simplified)

    (0 until InputBits) foreach { bit =>
      testAddition(bit, swapped)
    }

    swaps
      .flatMap { case (a, b) =>
        List(a, b)
      }
      .sorted
      .mkString(",")
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/24$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
