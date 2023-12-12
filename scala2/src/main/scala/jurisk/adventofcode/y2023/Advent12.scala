package jurisk.adventofcode.y2023

import jurisk.adventofcode.y2023.Advent12.Condition.{
  Damaged,
  Operational,
  Unknown,
}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

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

  private def splitBySeparator[T](l: List[T], sep: T): List[List[T]] =
    (l.span(_ != sep) match {
      case (hd, _ :: tl) => hd :: splitBySeparator(tl, sep)
      case (hd, _)       => List(hd)
    }).filter(_.nonEmpty)

  private def printL(l: List[Condition]): String =
    l.map(_.symbol).mkString

  val memo: mutable.Map[(List[List[NonOperational]], List[Int]), Long] =
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
          case nextSprings :: otherSprings =>
            val nextDamaged = nextSprings.head == Damaged

            val groupCanStartHere = nextSprings.length >= nextGroup
            val validSpaceAfter   = nextSprings.lift(nextGroup) != Some(Damaged)

            val mult1 = if (groupCanStartHere && validSpaceAfter) {
              val updated = nextSprings.drop(nextGroup + 1)
              val rem     = if (updated.nonEmpty) {
                updated :: otherSprings
              } else {
                otherSprings
              }

              arr(rem, otherGroups)
            } else {
              0
            }

            val mult2 = if (nextDamaged) { 0 }
            else {
              val updated = nextSprings.drop(1)
              val rem     = if (updated.nonEmpty) {
                updated :: otherSprings
              } else {
                otherSprings
              }

              arr(rem, nextGroup :: otherGroups)
            }

            mult1 + mult2

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

//    println(s"`${springs.map(x => printL(x))}` $groups => $result")

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
        splitBySeparator(conditions, Operational).map { list =>
          list.map {
            case x: NonOperational => x
            case x                 => x.toString.fail
          }
        }
//
//      println(conditions)
//      println(grouped)
//      println(groups)
//      println()

      arr(grouped, groups)
    }
  }

  object Row {
    def parse(s: String): Row = {
//      println(s)

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
