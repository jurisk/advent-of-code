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
  private val StartWorkflowName: WorkflowName  = "in"
  private val AcceptWorkflowName: WorkflowName = "A"
  private val RejectWorkflowName: WorkflowName = "R"

  sealed trait Criterion
  object Criterion {
    case object Accepted                              extends Criterion
    case object Rejected                              extends Criterion
    final case class Forward(forwardTo: WorkflowName) extends Criterion
    final case class LessThan(
      dimension: Dimension,
      compareWith: Long,
      workflow: WorkflowName,
    ) extends Criterion
    final case class GreaterThan(
      dimension: Dimension,
      compareWith: Long,
      workflow: WorkflowName,
    ) extends Criterion

    def parse(input: String): Criterion =
      input match {
        case s"$bef1>$bef2:$after" =>
          GreaterThan(Dimension.parse(bef1), bef2.toLong, after)

        case s"$bef1<$bef2:$after" =>
          LessThan(Dimension.parse(bef1), bef2.toLong, after)

        case AcceptWorkflowName => Accepted
        case RejectWorkflowName => Rejected

        case other => Forward(other)
      }
  }

  final case class Workflow(criteria: List[Criterion])

  object Workflow {
    def parse(input: String): Workflow =
      Workflow(input.split(",").toList.map(Criterion.parse))
  }

  final case class Input(
    workflows: Map[WorkflowName, Workflow],
    parts: List[Part],
  ) {
    def validPart(part: Part): Boolean = {
      @tailrec
      def resolve2(criteria: List[Criterion]): Boolean =
        criteria match {
          case head :: tail =>
            head match {
              case Criterion.Accepted                    => true
              case Criterion.Rejected                    => false
              case Criterion.Forward(forwardTo)          => resolve(forwardTo)
              case Criterion.LessThan(a, b, workflow)    =>
                if (part(a) < b) resolve(workflow)
                else resolve2(tail)
              case Criterion.GreaterThan(a, b, workflow) =>
                if (part(a) > b) resolve(workflow)
                else resolve2(tail)
            }
          case Nil          => "wtf".fail
        }

      def resolve(workflowName: WorkflowName): Boolean =
        workflowName match {
          case AcceptWorkflowName => true
          case RejectWorkflowName => false
          case other              =>
            val workflow = workflows(other)
            resolve2(workflow.criteria)

        }

      resolve(StartWorkflowName)
    }
  }

  def parse(input: String): Input = {
    val List(b, a) = input.split("\n\n").toList
    val parts      = a.parseLines(Part.parse)
    val workflows  = b.parseLines {
      case s"$name{$workflowString}" => (name, Workflow.parse(workflowString))
      case input                     => input.failedToParse
    }.toMap
    Input(workflows, parts)
  }

  def part1(data: Input): Long =
    data.parts.filter(data.validPart).map(_.sum).sum

  def splitAt(
    low: Part,
    high: Part,
    d: Dimension,
    n1: Long,
    n2: Long,
  ): ((Part, Part), (Part, Part)) = {
    assert(n1 + 1 == n2)

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
    val workflows = data.workflows

    def spaceSize2(low: Part, high: Part, criteria: List[Criterion]): Long =
      criteria match {
        case head :: tail =>
          head match {
            case Criterion.Accepted                    => spaceSize(low, high, AcceptWorkflowName)
            case Criterion.Rejected                    => spaceSize(low, high, RejectWorkflowName)
            case Criterion.Forward(forwardTo)          => spaceSize(low, high, forwardTo)
            case Criterion.LessThan(d, n, workflow)    =>
              val ((a1, a2), (b1, b2)) = splitAt(low, high, d, n - 1, n)
              spaceSize(a1, a2, workflow) +
                spaceSize2(b1, b2, tail)
            case Criterion.GreaterThan(d, n, workflow) =>
              val ((a1, a2), (b1, b2)) = splitAt(low, high, d, n, n + 1)
              spaceSize2(a1, a2, tail) +
                spaceSize(b1, b2, workflow)
          }

        case Nil => "wtf".fail
      }

    def spaceSize(low: Part, high: Part, workflowName: WorkflowName): Long =
      workflowName match {
        case AcceptWorkflowName =>
          spaceSizeRaw(low, high)

        case RejectWorkflowName =>
          0

        case other =>
          val workflow = workflows(other)
          spaceSize2(low, high, workflow.criteria)
      }

    val low  = Part(Dimension.All.map(_ -> 1L).toMap)
    val high = Part(Dimension.All.map(_ -> 4000L).toMap)

    spaceSize(low, high, StartWorkflowName)
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
