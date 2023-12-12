package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent12.Condition.{
  Damaged,
  Operational,
  Unknown,
}
import jurisk.utils.CollectionOps.{ListListOps, ListOps}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
import scala.collection.mutable

object Advent12 {
  type Input = List[Row]

  sealed trait Condition {
    def symbol: Char
    override def toString: String = symbol.toString
  }

  sealed trait NonOperational extends Condition

  object Condition {
    case object Operational extends Condition                     {
      override def symbol: Char = '.'
    }
    case object Damaged     extends Condition with NonOperational {
      override def symbol: Char = '#'
    }
    case object Unknown     extends Condition with NonOperational {
      override def symbol: Char = '?'
    }
  }

  private def printL(l: List[Condition]): String =
    l.map(_.symbol).mkString

  private val memo: mutable.Map[(List[List[NonOperational]], List[Int]), Long] =
    mutable.Map.empty

  private def arr(
    springs: List[List[NonOperational]],
    groups: List[Int],
  ): Long = {
    val maxPossibleDamaged = springs.map(_.size).sum
    val groupSum           = groups.sum

    if (groupSum > maxPossibleDamaged) {
      return 0
    }

    if (memo.contains((springs, groups))) {
      return memo((springs, groups))
    }

    val result = groups match {
      case nextGroup :: otherGroups =>
        springs match {
          case nextSprings :: _ =>
            val nextDamaged = nextSprings.head == Damaged

            val groupCanStartHere = nextSprings.length >= nextGroup
            val validSpaceAfter   = !nextSprings.lift(nextGroup).contains(Damaged)

            val startingHereOptions =
              if (groupCanStartHere && validSpaceAfter) {
                arr(
                  springs.dropFromFirstEliminatingEmpty(nextGroup + 1),
                  otherGroups,
                )
              } else {
                0
              }

            val skippingNextOptions = if (nextDamaged) {
              // Next is damaged, we cannot skip it
              0
            } else {
              // What if we skip the next one?
              arr(
                springs.dropFromFirstEliminatingEmpty(1),
                nextGroup :: otherGroups,
              )
            }

            startingHereOptions + skippingNextOptions

          case Nil => 0 // Group left but no matching springs
        }

      case Nil =>
        if (springs.forall(_.forall(_ == Unknown))) {
          // some springs left, but no groups left, but all those springs can be empty
          1
        } else {
          // we have damaged springs remaining but no groups left to cover
          0
        }
    }

    memo.put((springs, groups), result)

    result
  }

  final case class Row(
    conditions: List[Condition],
    groups: List[Int],
  ) {
    def expand(times: Int): Row =
      Row.parse(
        List.fill(times)(printL(conditions)).mkString("?") + " " + List
          .fill(times)(groups.map(_.toString).mkString(","))
          .mkString(",")
      )

    def expandedArrangements(times: Int): Long =
      expand(times).arrangements

    def arrangements: Long = {
      val grouped: List[List[NonOperational]] =
        conditions.splitBySeparator(Operational).map { list =>
          list.map {
            case x: NonOperational => x
            case x                 => x.toString.fail
          }
        }

      arr(grouped, groups)
    }
  }

  object Row {
    def parse(s: String): Row = {
      val (a, b)     = s.splitPairUnsafe(" ")
      val conditions = a.toList.map {
        case '.' => Operational
        case '#' => Damaged
        case '?' => Unknown
        case ch  => ch.toString.fail
      }

      val groups = b.extractIntList

      Row(conditions, groups)
    }

  }

  def parse(input: String): Input =
    input.parseLines(Row.parse)

  def part1(data: Input): Long =
    data.map(_.arrangements).sum

  def part2(data: Input): Long =
    data.map(_.expandedArrangements(5)).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/12.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
