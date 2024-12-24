package jurisk.adventofcode.y2024

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Parsing.StringOps

object Advent24 {
  type Wire = String
  type Input = (Map[Wire, Boolean], List[Operation])

 type N     = Long

  sealed trait Operation extends Product with Serializable {
    val a: Wire
    val b: Wire
    val out: Wire

    def wiresMentioned: Set[Wire] =
      this match {
        case Operation.And(a, b, out) => Set(a, b, out)
        case Operation.Or(a, b, out) => Set(a, b, out)
        case Operation.Xor(a, b, out) => Set(a, b, out)
      }
  }
  object Operation {
    final case class And(
      a: Wire,
      b: Wire,
      out: Wire,
    ) extends Operation

    final case class Or(
                          a: Wire,
                          b: Wire,
                          out: Wire,
                        ) extends Operation

    final case class Xor(
                          a: Wire,
                          b: Wire,
                          out: Wire,
                        ) extends Operation


    private val RegEx = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r
    def parse(s: String): Operation =
      s match {
        case RegEx(a, op, b, out)            =>
          op match {
            case "AND" => And(a, b, out)
            case "OR" => Or(a, b, out)
            case "XOR" => Xor(a, b, out)
            case _     => s.failedToParse
          }
        case _                 => s.failedToParse
      }
  }

  def parse(input: String): Input = {
    val (wiresS, operations) = input.splitPairByDoubleNewline
    val wires = wiresS.splitLines map { w: String =>
      val (q, bbg) = w.splitPairUnsafe(": ")
      val i = bbg.toInt
      val b = i match {
        case 0 => false
        case 1 => true
        case _ => "fail".fail
      }
      (q, b)
    }
    val ops = operations.splitLines map Operation.parse
    (wires.toMap, ops)
  }

  def resolveUnknown(wires: Map[Wire, Boolean], operations: List[Operation]): Map[Wire, Boolean] = {
    var results = wires
    var queue = wires.keySet ++ operations.toSet.flatMap { q: Operation => q.wiresMentioned }
    println(s"queue: ${queue.size}")
    var useful = true
    while (useful) {
      useful = false
      operations foreach { op =>
        if (results.contains(op.a) && results.contains(op.b) && !results.contains(op.out)) {
          op match {
            case Operation.And(a, b, out) =>
              results += (out -> (results(a) && results(b)))
            case Operation.Or(a, b, out) =>
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
    val results = resolveUnknown(wires, operations)
    val z = results.filter { case (k, v) => k.startsWith("z")}.toList
    val sor = z.sorted.map(_._2).map(b => if (b) "1" else "0").mkString.reverse
    println(sor)
    BigInt(sor, 2)
  }

  def part2(data: Input): N =
    0

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
