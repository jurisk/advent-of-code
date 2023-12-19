package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

object Advent19 {
  sealed trait Dimension
  object Dimension {
    case object X extends Dimension
    case object M extends Dimension
    case object A extends Dimension
    case object S extends Dimension

    val All: List[Dimension] = X :: M :: A :: S :: Nil

    def parse(input: String): Dimension = {
      assert(input.length == 1)
      input.head match {
        case 'x' => X
        case 'm' => M
        case 'a' => A
        case 's' => S
      }
    }
  }

  final case class Part(values: Map[Dimension, Long]) {
    def sum: Long                         = values.values.sum
    def apply(dimension: Dimension): Long = values(dimension)

    def updateToBeLessOrEqual(dimension: Dimension, n: Int): Part =
      Part(
        values.updated(
          dimension,
          apply(dimension).min(n),
        )
      )

    def updateToBeMoreOrEqual(dimension: Dimension, n: Int): Part =
      Part(
        values.updated(
          dimension,
          apply(dimension).max(n),
        )
      )

  }

  object Part {
    def parse(input: String): Part =
      input match {
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(
            Map(
              Dimension.X -> x.toInt,
              Dimension.M -> m.toInt,
              Dimension.A -> a.toInt,
              Dimension.S -> s.toInt,
            )
          )
        case _                        => input.failedToParse
      }
  }

  type RuleName = String
  val StartRuleName: RuleName = "in"

  sealed trait Criterion
  object Criterion {
    case object Accepted                     extends Criterion
    case object Rejected                     extends Criterion
    final case class Forward(rule: RuleName) extends Criterion
    final case class LessThan(a: Dimension, b: Int, rule: RuleName)
        extends Criterion
    final case class GreaterThan(a: Dimension, b: Int, rule: RuleName)
        extends Criterion

    def parse(input: String): Criterion =
      input match {
        case s"$bef1>$bef2:$after" =>
          GreaterThan(Dimension.parse(bef1), bef2.toInt, after)

        case s"$bef1<$bef2:$after" =>
          LessThan(Dimension.parse(bef1), bef2.toInt, after)

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
                if (part(a) < b) resolve(rule)
                else resolve2(tail)
              case Criterion.GreaterThan(a, b, rule) =>
                if (part(a) > b) resolve(rule)
                else resolve2(tail)
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

  def part1(data: Input): Long =
    data.parts.filter(data.validPart).map(_.sum).sum

  def splitAt(
    low: Part,
    high: Part,
    d: Dimension,
    n1: Int,
    n2: Int,
  ): ((Part, Part), (Part, Part)) = {
    val beforeChecksum = spaceSizeRaw(low, high)

    val a1 = low
    val a2 = high.updateToBeLessOrEqual(d, n1)

    val b1 = low.updateToBeMoreOrEqual(d, n2)
    val b2 = high

    val aChecksum = spaceSizeRaw(a1, a2)
    val bChecksum = spaceSizeRaw(b1, b2)

    assert(beforeChecksum == aChecksum + bChecksum)

    ((a1, a2), (b1, b2))
  }

  def spaceSizeRaw(low: Part, high: Part): Long =
    Dimension.All.map(d => high(d) - low(d) + 1).product

  def part2(data: Input): Long = {
    val rules = data.rules

    def spaceSize2(low: Part, high: Part, criteria: List[Criterion]): Long =
      criteria match {
        case head :: tail =>
          head match {
            case Criterion.Accepted                => spaceSize(low, high, "A")
            case Criterion.Rejected                => spaceSize(low, high, "R")
            case Criterion.Forward(forwardTo)      => spaceSize(low, high, forwardTo)
            case Criterion.LessThan(d, n, rule)    =>
              val ((a1, a2), (b1, b2)) = splitAt(low, high, d, n - 1, n)
              spaceSize(a1, a2, rule) +
                spaceSize2(b1, b2, tail)
            case Criterion.GreaterThan(d, n, rule) =>
              val ((a1, a2), (b1, b2)) = splitAt(low, high, d, n, n + 1)
              spaceSize2(a1, a2, tail) +
                spaceSize(b1, b2, rule)
          }

        case Nil => "wtf".fail
      }

    def spaceSize(low: Part, high: Part, ruleName: RuleName): Long =
      ruleName match {
        case "A" =>
          spaceSizeRaw(low, high)

        case "R" =>
          0

        case other =>
          val rule = rules(other)
          spaceSize2(low, high, rule.criteria)
      }

    val startD = 1
    val endD   = 4000
    val low    = Part(
      Map(
        Dimension.X -> startD,
        Dimension.M -> startD,
        Dimension.A -> startD,
        Dimension.S -> startD,
      )
    )
    val high   = Part(
      Map(
        Dimension.X -> endD,
        Dimension.M -> endD,
        Dimension.A -> endD,
        Dimension.S -> endD,
      )
    )

    spaceSize(low, high, StartRuleName)
  }

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
