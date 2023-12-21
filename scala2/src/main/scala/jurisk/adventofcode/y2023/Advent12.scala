package jurisk.adventofcode.y2023

import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import jurisk.adventofcode.y2023.Condition._
import jurisk.math.Combinatorics
import jurisk.utils.CollectionOps.ListOps
import jurisk.utils.FileInputIO.readFileText
import jurisk.utils.Memoize.memoize2
import jurisk.utils.Parsing.StringOps

sealed trait Condition {
  def symbol: Char
  override def toString: String = symbol.toString
}

object Condition {
  case object Operational extends Condition {
    override def symbol: Char = '.'
  }
  case object Damaged     extends Condition {
    override def symbol: Char = '#'
  }
  case object Unknown     extends Condition {
    override def symbol: Char = '?'
  }
}

object Advent12 extends IOApp.Simple {
  type Input = List[Row]

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
      lazy val fMemoized: (Int, Int) => Long = memoize2(f)

      def f(at: Int, dropGroups: Int): Long = {
        val springs    = conditions.drop(at)
        val groupsLeft = groups.drop(dropGroups)

        val result = groupsLeft match {
          case nextGroup :: _ =>
            val startingHereOptions = {
              // Do we even have enough space to start the next group here?
              val groupCanStartHere = springs.size >= nextGroup && !springs
                .take(nextGroup)
                .contains(Operational)

              // If we start a group here, the next one has to be free
              val validSpaceAfter =
                !springs.lift(nextGroup).contains(Damaged)

              if (groupCanStartHere && validSpaceAfter) {
                fMemoized(at + nextGroup + 1, dropGroups + 1)
              } else {
                0
              }
            }

            val skippingNextOptions =
              springs.headOption match {
                case Some(Operational) | Some(Unknown) =>
                  // What if we skip the next one?
                  fMemoized(at + 1, dropGroups)
                case Some(Damaged)                     =>
                  // We cannot skip this one
                  0
                case None                              =>
                  0
              }

            startingHereOptions + skippingNextOptions

          case Nil =>
            if (springs.contains(Damaged)) {
              // we have damaged springs remaining but no groups left to cover
              0
            } else {
              // some springs left, but no groups left, but all those springs can be empty
              1
            }
        }

        result
      }

      f(0, 0)
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

  def solve(data: Input, times: Int): IO[Long] =
    data.parTraverse(x => IO(x.expand(times).arrangements)).map(_.sum)

  def part1(data: Input): IO[Long] =
    solve(data, 1)

  def part2(data: Input): IO[Long] =
    solve(data, 5)

  def parseFile(fileName: String): IO[Input] = for {
    input <- readFileText(fileName)
  } yield parse(input)

  override def run: IO[Unit] = for {
    realData <- parseFile("2023/12.txt")

    result1 <- part1(realData)
    _       <- IO.println(s"Part 1: $result1")

    result2 <- part2(realData)
    _       <- IO.println(s"Part 2: $result2")
  } yield ()
}
