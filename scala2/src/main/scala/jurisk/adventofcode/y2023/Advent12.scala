package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent12.Condition._
import jurisk.math.Combinatorics
import jurisk.utils.CollectionOps.{ListListOps, ListOps}
import jurisk.utils.FileInput._
import jurisk.utils.Memoize
import jurisk.utils.Parsing.StringOps

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

  private val calculateArrangementsMemoized
    : ((List[List[NonOperational]], List[Int])) => Long =
    Memoize.memoize(calculateArrangements)

  private def calculateArrangements(
    input: (List[List[NonOperational]], List[Int])
  ): Long = {
    val (springs, groups) = input

    val maxPossibleDamaged = springs.map(_.size).sum
    val groupSum           = groups.sum

    if (groupSum > maxPossibleDamaged) {
      // We don't have enough possible springs left in `springs` to cover all the groups in `groups`, it is hopeless
      0
    } else {
      groups match {
        case nextGroup :: otherGroups =>
          springs match {
            case nextSprings :: _ =>
              val startingHereOptions = {
                // Do we even have enough space to start the next group here?
                val groupCanStartHere = nextSprings.length >= nextGroup

                // If we start a group here, the next one has to be free
                val validSpaceAfter =
                  !nextSprings.lift(nextGroup).contains(Damaged)

                if (groupCanStartHere && validSpaceAfter) {
                  calculateArrangementsMemoized(
                    (
                      springs.dropFromFirstEliminatingEmpty(nextGroup + 1),
                      otherGroups,
                    )
                  )
                } else {
                  0
                }
              }

              val skippingNextOptions =
                if (nextSprings.headOption.contains(Damaged)) {
                  // Next is damaged, we cannot skip it and must start the group here
                  0
                } else {
                  // What if we skip the next one?
                  calculateArrangementsMemoized(
                    (
                      springs.dropFromFirstEliminatingEmpty(1),
                      nextGroup :: otherGroups,
                    )
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
    }
  }

  final case class Row(
    conditions: List[Condition],
    groups: List[Int],
  ) {
    def expand(times: Int): Row = Row(
      conditions.multiplyAndFlattenWithSeparator(times, Unknown),
      groups.multiplyAndFlatten(times),
    )

    // Not actually used except by tests, shown for illustration
    def bruteForceArrangements: Int =
      Combinatorics
        .fillWildcards(conditions, Unknown, List(Damaged, Operational))
        .count { option =>
          option.splitBySeparator(Operational).map(_.length) == groups
        }

    def arrangements: Long = {
      val grouped: List[List[NonOperational]] =
        conditions.splitBySeparator(Operational).map { list =>
          list.map {
            case x: NonOperational => x
            case x                 => s"Did not expect $x in split conditions".fail
          }
        }

      calculateArrangements((grouped, groups))
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

  def solve(data: Input, times: Int): Long =
    data.map(_.expand(times).arrangements).sum

  def part1(data: Input): Long =
    solve(data, 1)

  def part2(data: Input): Long =
    solve(data, 5)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/12.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
