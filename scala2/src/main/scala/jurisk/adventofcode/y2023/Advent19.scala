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

    def updateToBeLessOrEqual(dimension: Dimension, n: Long): Part =
      Part(
        values.updated(
          dimension,
          apply(dimension).min(n),
        )
      )

    def updateToBeMoreOrEqual(dimension: Dimension, n: Long): Part =
      Part(
        values.updated(
          dimension,
          apply(dimension).max(n),
        )
      )
  }

  final case class PartSpace(low: Part, high: Part)

  object Part {
    def parse(input: String): Part =
      input match {
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(
            (Dimension.All zip List(x, m, a, s)).map { case (d, n) =>
              d -> n.toLong
            }.toMap
          )
        case _                        => input.failedToParse
      }
  }

  private type WorkflowName = String
  private val StartWorkflowName: WorkflowName = "in"

  sealed trait Rule
  object Rule {
    case object Accepted                              extends Rule
    case object Rejected                              extends Rule
    final case class Forward(forwardTo: WorkflowName) extends Rule
    final case class LessThan(
      dimension: Dimension,
      compareWith: Long,
      workflow: WorkflowName,
    ) extends Rule
    final case class GreaterThan(
      dimension: Dimension,
      compareWith: Long,
      workflow: WorkflowName,
    ) extends Rule

    def parse(input: String): Rule =
      input match {
        case s"$bef1>$bef2:$after" =>
          GreaterThan(Dimension.parse(bef1), bef2.toLong, after)

        case s"$bef1<$bef2:$after" =>
          LessThan(Dimension.parse(bef1), bef2.toLong, after)

        case other => Forward(other)
      }
  }

  final case class Workflow(rules: List[Rule])

  object Workflow {
    def parse(input: String): Workflow =
      Workflow(input.split(",").toList.map(Rule.parse))
  }

  final case class Input(
    workflows: Map[WorkflowName, Workflow],
    parts: List[Part],
  ) {
    def validPart(part: Part): Boolean = {
      @tailrec
      def resolveRules(rules: List[Rule]): Boolean =
        rules match {
          case head :: tail =>
            head match {
              case Rule.Accepted                    => true
              case Rule.Rejected                    => false
              case Rule.Forward(forwardTo)          => resolveWorkflow(forwardTo)
              case Rule.LessThan(a, b, workflow)    =>
                if (part(a) < b) resolveWorkflow(workflow)
                else resolveRules(tail)
              case Rule.GreaterThan(a, b, workflow) =>
                if (part(a) > b) resolveWorkflow(workflow)
                else resolveRules(tail)
            }
          case Nil          => "Ran out of rules".fail
        }

      def resolveWorkflow(workflowName: WorkflowName): Boolean =
        workflowName match {
          case other =>
            val workflow = workflows(other)
            resolveRules(workflow.rules)
        }

      resolveWorkflow(StartWorkflowName)
    }
  }

  object Input {
    def parse(input: String): Input = {
      val List(b, a) = input.split("\n\n").toList
      val parts      = a.parseLines(Part.parse)

      val parsedWorkflows = b.parseLines {
        case s"$name{$workflowString}" => (name, Workflow.parse(workflowString))
        case input                     => input.failedToParse
      }.toMap

      val syntheticWorkflows = Map(
        "A" -> Workflow(Rule.Accepted :: Nil),
        "R" -> Workflow(Rule.Rejected :: Nil),
      )

      Input(parsedWorkflows ++ syntheticWorkflows, parts)
    }
  }

  def part1(data: Input): Long =
    data.parts.filter(data.validPart).map(_.sum).sum

  // Providing two cut points 1 unit apart even though we could have just one, but this seemed
  // less error-prone
  def splitAt(
    space: PartSpace,
    d: Dimension,
    cutAtLower: Long,
    cutAtHigher: Long,
  ): (PartSpace, PartSpace) = {
    assert(cutAtLower + 1 == cutAtHigher)

    val a = space.copy(high = space.high.updateToBeLessOrEqual(d, cutAtLower))
    val b = space.copy(low = space.low.updateToBeMoreOrEqual(d, cutAtHigher))

    assert(spaceSizeRaw(space) == spaceSizeRaw(a) + spaceSizeRaw(b))

    (a, b)
  }

  def spaceSizeRaw(space: PartSpace): Long =
    Dimension.All.map(d => space.high(d) - space.low(d) + 1).product

  def part2(data: Input): Long = {
    val workflows = data.workflows

    def ruleSpaceSize(space: PartSpace, rules: List[Rule]): Long =
      rules match {
        case head :: tail =>
          head match {
            case Rule.Accepted                    =>
              spaceSizeRaw(space)
            case Rule.Rejected                    =>
              0
            case Rule.Forward(forwardTo)          =>
              workflowSpaceSize(space, forwardTo)
            case Rule.LessThan(d, n, workflow)    =>
              val (a, b) = splitAt(space, d, n - 1, n)
              workflowSpaceSize(a, workflow) + ruleSpaceSize(b, tail)
            case Rule.GreaterThan(d, n, workflow) =>
              val (a, b) = splitAt(space, d, n, n + 1)
              ruleSpaceSize(a, tail) + workflowSpaceSize(b, workflow)
          }

        case Nil => "Ran out of rules".fail
      }

    def workflowSpaceSize(space: PartSpace, workflowName: WorkflowName): Long =
      ruleSpaceSize(space, workflows(workflowName).rules)

    val low  = Part(Dimension.All.map(_ -> 1L).toMap)
    val high = Part(Dimension.All.map(_ -> 4000L).toMap)

    workflowSpaceSize(PartSpace(low, high), StartWorkflowName)
  }

  def parseFile(fileName: String): Input =
    Input.parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/19$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
