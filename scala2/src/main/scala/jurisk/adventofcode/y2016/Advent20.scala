package jurisk.adventofcode.y2016

import cats.effect.IO
import cats.effect.IOApp
import jurisk.math.DiscreteInterval
import jurisk.math.DiscreteIntervalSet
import jurisk.utils.CollectionOps.OptionOps
import jurisk.utils.FileInputIO.readFileLines
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent20 extends IOApp.Simple {
  private def parseLines(
    lines: List[String]
  ): List[DiscreteInterval[Long]] =
    lines map {
      case s"$a-$b" => DiscreteInterval(a.toLong, b.toLong)
      case s        => s.failedToParse
    }

  private def solve(
    lines: List[String]
  ): DiscreteIntervalSet[Long] = {
    val data = parseLines(lines)
    val min  = data.map(_.from).min
    val max  = data.map(_.from).max
    data.foldLeft(DiscreteIntervalSet.fromInterval(min, max))(
      _ subtract _
    )
  }

  private def part1(lines: List[String]): Long =
    solve(lines).minOption.orFail("Failed to solve")

  private def part2(lines: List[String]): Long =
    solve(lines).size

  override def run: IO[Unit] = for {
    testData   <- readFileLines("2016/20-test.txt")
    testResult1 = part1(testData)
    _           = testResult1 shouldEqual 3
    realData   <- readFileLines("2016/20.txt")
    realResult1 = part1(realData)
    _          <- IO.println(s"Part 1: $realResult1")
    _           = realResult1 shouldEqual 14975795
    realResult2 = part2(realData)
    _          <- IO.println(s"Part 2: $realResult2")
    _           = realResult2 shouldEqual 101
  } yield ()
}
