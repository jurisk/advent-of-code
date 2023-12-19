package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent19 {
  final case class Part(x: Int, m: Int, a: Int, s: Int) {
    def sum: Int                 = x + m + a + s
    def getByChar(ch: Char): Int =
      ch match {
        case 'x' => x
        case 'm' => m
        case 'a' => a
        case 's' => s
      }
  }

  object Part {
    def parse(input: String): Part =
      input match {
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(x.toInt, m.toInt, a.toInt, s.toInt)
        case _                        => input.failedToParse
      }
  }

  type RuleName = String
  val StartRuleName: RuleName = "in"

  sealed trait Criterion
  object Criterion {
    case object Accepted                                       extends Criterion
    case object Rejected                                       extends Criterion
    final case class Forward(rule: RuleName)                   extends Criterion
    final case class LessThan(a: Char, b: Int, rule: RuleName) extends Criterion
    final case class GreaterThan(a: Char, b: Int, rule: RuleName)
        extends Criterion

    def parse(input: String): Criterion =
      input match {
        case s"$bef1>$bef2:$after" =>
          assert(bef1.length == 1)
          GreaterThan(bef1.head, bef2.toInt, after)

        case s"$bef1<$bef2:$after" =>
          assert(bef1.length == 1)
          LessThan(bef1.head, bef2.toInt, after)

        case "A" => Accepted
        case "R" => Rejected

        case other => Forward(other)
      }
  }

  final case class Rule(criteria: List[Criterion])

  object Rule {
    def parse(input: String): Rule =
      Rule(input.split(",").toList.map(Criterion.parse))
  }

  final case class Input(
    rules: Map[RuleName, Rule],
    parts: List[Part],
  ) {
    def validPart(part: Part): Boolean = {
      @tailrec
      def resolve2(criteria: List[Criterion]): Boolean =
        criteria match {
          case head :: tail =>
            head match {
              case Criterion.Accepted                => true
              case Criterion.Rejected                => false
              case Criterion.Forward(rule)           => resolve(rule)
              case Criterion.LessThan(a, b, rule)    =>
                if (part.getByChar(a) < b) resolve(rule) else resolve2(tail)
              case Criterion.GreaterThan(a, b, rule) =>
                if (part.getByChar(a) > b) resolve(rule) else resolve2(tail)
            }
          case Nil          => "wtf".fail
        }

      def resolve(ruleName: RuleName): Boolean =
        ruleName match {
          case "A"   => true
          case "R"   => false
          case other =>
            val rule = rules(other)
            resolve2(rule.criteria)

        }

      resolve(StartRuleName)
    }
  }

  def parse(input: String): Input = {
    val List(b, a) = input.split("\n\n").toList
    val parts      = a.parseLines(Part.parse)
    val rules      = b.parseLines {
      case s"$name{$ruleString}" => (name, Rule.parse(ruleString))
      case input                 => input.failedToParse
    }.toMap
    Input(rules, parts)
  }

  def part1(data: Input): Int =
    data.parts.filter(data.validPart).map(_.sum).sum

  def part2(data: Input): Int =
    0

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/19$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}