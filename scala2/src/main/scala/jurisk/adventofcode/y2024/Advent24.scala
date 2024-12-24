package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.FileInputIO
import jurisk.utils.Parsing.StringOps

object Advent24 {
  private type Wire = String
  type Input        = (Map[Wire, Boolean], Set[Operation])

  type N = Long

  sealed trait Operation extends Product with Serializable {
    val a: Wire
    val b: Wire
    val out: Wire

    def name: String

    def rename(what: String, toWhat: String): Operation

    def wiresMentioned: Set[Wire] =
      this match {
        case Operation.And(a, b, out) => Set(a, b, out)
        case Operation.Or(a, b, out)  => Set(a, b, out)
        case Operation.Xor(a, b, out) => Set(a, b, out)
      }
  }
  object Operation {
    final case class And(
      a: Wire,
      b: Wire,
      out: Wire,
    ) extends Operation {
      def name: String = s"$a AND $b"

      override def rename(what: Wire, toWhat: Wire): Operation =
        if (a == what) {
          And(toWhat, b, out)
        } else if (b == what) {
          And(a, toWhat, out)
        } else if (out == what) {
          And(a, b, toWhat)
        } else {
          this
        }
    }

    final case class Or(
      a: Wire,
      b: Wire,
      out: Wire,
    ) extends Operation {
      def name: String = s"$a OR $b"

      override def rename(what: Wire, toWhat: Wire): Operation =
        if (a == what) {
          Or(toWhat, b, out)
        } else if (b == what) {
          Or(a, toWhat, out)
        } else if (out == what) {
          Or(a, b, toWhat)
        } else {
          this
        }
    }

    final case class Xor(
      a: Wire,
      b: Wire,
      out: Wire,
    ) extends Operation {
      def name: String = s"$a XOR $b"

      override def rename(what: Wire, toWhat: Wire): Operation =
        if (a == what) {
          Xor(toWhat, b, out)
        } else if (b == what) {
          Xor(a, toWhat, out)
        } else if (out == what) {
          Xor(a, b, toWhat)
        } else {
          this
        }
    }

    private val RegEx               = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r
    def parse(s: String): Operation =
      s match {
        case RegEx(a, op, b, out) =>
          op match {
            case "AND" => And(a, b, out)
            case "OR"  => Or(a, b, out)
            case "XOR" => Xor(a, b, out)
            case _     => s.failedToParse
          }
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
    val ops                  = operations.splitLines.toSet map Operation.parse
    (wires.toMap, ops)
  }

  def resolveUnknown(
    wires: Map[Wire, Boolean],
    operations: Set[Operation],
  ): Map[Wire, Boolean] = {
    var results = wires
    var queue   = wires.keySet ++ operations.toSet.flatMap { q: Operation =>
      q.wiresMentioned
    }
    var useful  = true
    while (useful) {
      useful = false
      operations foreach { op =>
        if (
          results.contains(op.a) && results
            .contains(op.b) && !results.contains(op.out)
        ) {
          op match {
            case Operation.And(a, b, out) =>
              results += (out -> (results(a) && results(b)))
            case Operation.Or(a, b, out)  =>
              results += (out -> (results(a) || results(b)))
            case Operation.Xor(a, b, out) =>
              results += (out -> (results(a) ^ results(b)))
          }
          queue -= op.out
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

  private def debugWrite(operations: Set[Operation]): Unit = {
    val allWires = operations.flatMap { q: Operation => q.wiresMentioned }
    val ops      = operations map { op =>
      val opName = s""""${op.name}""""
      s"""
         | ${op.a} -> $opName
         | ${op.b} -> $opName
         | $opName -> ${op.out}
         |
         |""".stripMargin
//        s"""
//           | ${op.a} -> ${op.out} [ label=${opName} ];
//           | ${op.b} -> ${op.out} [ label=${opName} ];
//           |
//           |""".stripMargin

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
      .map(w => s"""$w [shape=box, color=red];""")
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

  private def testAddition(bit: Int, operations: Set[Operation]): Unit =
    List(
      (false, false, false),
      (false, true, true),
      (true, false, true),
      (true, true, false),
    ) foreach { case (x, y, r) =>
      val m = zeroWires ++ Map(xReg(bit) -> x, yReg(bit) -> y)
      val o = resolveUnknown(m, operations)
      if (o.getOrElse(zReg(bit), false) != r) {
        println(s"bit: $bit, x: $x, y: $y, r: $r")
        println(s"o: $o")
      }
    }

  val InputBits  = 45
  val OutputBits = InputBits + 1

  def rename(ops: Set[Operation], what: Wire, toWhat: Wire): Set[Operation] = {
    println(s"renaming $what to $toWhat")
    ops.map(_.rename(what, toWhat))
  }

  def renameOperations(operations: Set[Operation]): Set[Operation] = {
    val q = (0 until InputBits).foldLeft(operations) { case (ops, bit) =>
      ops.find {
        case Operation.And(a, b, _) => a == xReg(bit) && b == yReg(bit)
        case _                      => false
      } match {
        case Some(found) =>
          rename(ops, found.out, f"u$bit%02d")
        case None        => ops
      }
    }

    val r = (0 until InputBits).foldLeft(q) { case (ops, bit) =>
      ops.find {
        case Operation.Or(a, b, _) => a == xReg(bit) && b == yReg(bit)
        case _                     => false
      } match {
        case Some(found) =>
          rename(ops, found.out, f"i$bit%02d")
        case None        => ops
      }
    }

    (0 until InputBits).foldLeft(r) { case (ops, bit) =>
      ops.find {
        case Operation.Xor(a, b, _) => a == xReg(bit) && b == yReg(bit)
        case _                      => false
      } match {
        case Some(found) =>
          rename(ops, found.out, f"q$bit%02d")
        case None        => ops
      }
    }
  }

  private def simplifyBit(
    operations: Set[Operation],
    bit: Int,
  ): Set[Operation] = {
    val foundAnd = operations.find { op =>
      op match {
        case Operation.And(a, b, _) =>
          (a == xReg(bit) && b == yReg(bit)) || (a == yReg(bit) && b == xReg(
            bit
          ))
        case _                      => false
      }
    }

    val intermediate = foundAnd match {
      case Some(a @ Operation.And(_, _, out)) =>
        rename(operations - a, out, f"AND_$bit%02d")
      case _                                  =>
        println(s"bit: $bit, AND not found")
        operations
    }

    val foundXor = intermediate.find { op =>
      op match {
        case Operation.Xor(a, b, _) =>
          (a == xReg(bit) && b == yReg(bit)) || (a == yReg(bit) && b == xReg(
            bit
          ))
        case _                      => false
      }
    }

    foundXor match {
      case Some(a @ Operation.Xor(_, _, out)) if out != "z00" =>
        rename(intermediate - a, out, f"XOR_$bit%02d")
      case _                                                  =>
        println(s"bit: $bit, XOR not found")
        intermediate
    }
  }

  def simplify(operations: Set[Operation]): Set[Operation] =
    (0 until InputBits).foldLeft(operations) { case (ops, bit) =>
      simplifyBit(ops, bit)
    }

  def part2(data: Input): String = {
    val (wires, operations) = data
    val simplified          = simplify(operations)
    debugWrite(simplified)

//    (0 until InputBits) foreach { bit =>
//      testAddition(bit, simplified)
//    }

    "asdf"
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
