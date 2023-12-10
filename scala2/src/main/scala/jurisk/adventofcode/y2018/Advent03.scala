package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput.parseFileLines
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent03 {
  type ClaimId = Int

  final case class Claim(id: ClaimId, area: Area2D)
  object Claim {
    private val RegEx           = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    def parse(s: String): Claim =
      s match {
        case RegEx(id, left, top, width, height) =>
          Claim(
            id.toInt,
            Area2D.fromLeftTopWidthHeight(
              left.toInt,
              top.toInt,
              width.toInt,
              height.toInt,
            ),
          )
        case _                                   => s.failedToParse
      }
  }

  def readFileAndParse(fileName: String): List[Claim] =
    parseFileLines(fileName, Claim.parse)

  private def aggregate(data: List[Claim]): Map[Coords2D, Int] =
    data.foldLeft(Map.empty[Coords2D, Int]) { case (acc, claim) =>
      claim.area.points.foldLeft(acc) { case (mapAcc, point) =>
        mapAcc.updatedWith(point) { oldValue =>
          (oldValue.getOrElse(0) + 1).some
        }
      }
    }

  def part1(data: List[Claim]): Int =
    aggregate(data) count { case (_, n) =>
      n >= 2
    }

  def part2(data: List[Claim]): Int = {
    val counts = aggregate(data)

    val nonOverlapping = data filter { claim =>
      claim.area.points forall { coords =>
        counts(coords) == 1
      }
    }

    nonOverlapping.singleElementUnsafe.id
  }

  def main(args: Array[String]): Unit = {
    val real = readFileAndParse("2018/03.txt")
    val test = readFileAndParse("2018/03-test.txt")

    part1(test) shouldEqual 4
    part1(real) shouldEqual 105231

    part2(test) shouldEqual 3
    part2(real) shouldEqual 164
  }
}
