package jurisk.adventofcode.y2024

import jurisk.adventofcode.y2024.Advent24.Operation.And
import jurisk.adventofcode.y2024.Advent24.Operation.Or
import jurisk.adventofcode.y2024.Advent24.Operation.Xor
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

    def name: String = s"$a ${op.toString.toUpperCase} $b"
  }

  sealed trait Operation extends Product with Serializable
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

          Connection(lowest, highest, operation, out)
        case _                    => s.failedToParse
      }
  }

  def parse(input: String): Input = {
    val (wiresS, operations) = input.splitPairByDoubleNewline
    val wires                = wiresS.splitLines map { w: String =>
      val (q, bbg) = w.splitPairUnsafe(": ")
      val i        = bbg.toInt
      val b        = i match {
        case 0 => false
        case 1 => true
        case _ => "fail".fail
      }
      (q, b)
    }
    val ops                  = operations.splitLines.toSet map Connection.parse
    (wires.toMap, ops)
  }

  def resolveUnknown(
    wires: Map[Wire, Boolean],
    connections: Set[Connection],
  ): Map[Wire, Boolean] = {
    var results = wires
    var queue   = wires.keySet ++ connections.flatMap { q: Connection =>
      q.wiresMentioned
    }
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
    val (wires, operations) = data
    val results             = resolveUnknown(wires, operations)
    val z                   = results.filter { case (k, _) => k.startsWith("z") }.toList
    val sor                 = z.sorted.map(_._2).map(b => if (b) "1" else "0").mkString.reverse
    println(sor)
    BigInt(sor, 2)
  }

  private def debugWrite(operations: Set[Connection]): Unit = {
    val allWires = operations.flatMap { q: Connection => q.wiresMentioned }
    val ops      = operations map { op =>
      val opName = s""""${op.name}""""
      s"""
         | ${op.a} -> $opName
         | ${op.b} -> $opName
         | $opName -> ${op.out}
         |
         |""".stripMargin
    }

    val xNodes = allWires
      .filter(_.startsWith("x"))
      .map(w => s"""$w [shape=box, color=blue];""")
      .mkString("\n")
    val yNodes = allWires
      .filter(_.startsWith("y"))
      .map(w => s"""$w [shape=box, color=green];""")
      .mkString("\n")
    val zNodes = allWires
      .filter(_.startsWith("z"))
      .map { w =>
        s"""$w [shape=box, color=red];"""
      }
      .mkString("\n")

    val output = s"""
                    |digraph G {
                    | $xNodes
                    | $yNodes
                    | $zNodes
                    |
                    | ${ops.mkString}
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

  private def testAddition(bit: Int, operations: Set[Connection]): Unit =
    List(
      (false, false, false, false),
      (false, true, true, false),
      (true, false, true, false),
      (true, true, false, true),
    ) foreach { case (x, y, r, c) =>
      val m         = zeroWires ++ Map(xReg(bit) -> x, yReg(bit) -> y)
      val o         = resolveUnknown(m, operations)
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

  def rename(
    ops: Set[Connection],
    what: Wire,
    toWhat: Wire,
  ): Set[Connection] = {
    println(s"renaming $what to $toWhat")
    ops.map(_.rename(what, toWhat))
  }

  private def simplifyBit(
    operations: Set[Connection],
    bit: Int,
  ): Set[Connection] = {
    val foundAnd = operations.find { op =>
      op match {
        case Connection(a, b, And, _) =>
          (a == xReg(bit) && b == yReg(bit)) || (a == yReg(bit) && b == xReg(
            bit
          ))
        case _                        => false
      }
    }

    val intermediate = foundAnd match {
      case Some(a @ Connection(_, _, And, out)) =>
        rename(operations - a, out, f"AND_$bit%02d")
      case _                                    =>
        println(s"bit: $bit, AND not found")
        operations
    }

    val foundXor = intermediate.find { op =>
      op match {
        case Connection(a, b, Xor, _) =>
          (a == xReg(bit) && b == yReg(bit)) || (a == yReg(bit) && b == xReg(
            bit
          ))
        case _                        => false
      }
    }

    foundXor match {
      case Some(a @ Connection(_, _, Xor, out)) if out != "z00" =>
        rename(intermediate - a, out, f"XOR_$bit%02d")
      case _                                                    =>
        println(s"bit: $bit, XOR not found")
        intermediate
    }
  }

  def simplify(operations: Set[Connection]): Set[Connection] =
    (0 until InputBits).foldLeft(operations) { case (ops, bit) =>
      simplifyBit(ops, bit)
    }

  def part2(data: Input): String = {
    val (_, operations) = data

    val swaps =
      List(("hbk", "z14"), ("kvn", "z18"), ("dbb", "z23"), ("cvh", "tfn"))

    val swapped = swaps.foldLeft(operations) { case (acc, (a, b)) =>
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
