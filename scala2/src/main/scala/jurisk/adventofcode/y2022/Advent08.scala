package jurisk.adventofcode.y2022

import cats.data.NonEmptyList
import jurisk.utils.FileInput._
import jurisk.utils.Geometry.{Coords2D, Field2D}
import org.scalatest.matchers.should.Matchers._

object Advent08 {
  type TreeHeight = Int
  type Parsed     = Field2D[TreeHeight]
  type Processed  = Parsed
  type Result1    = Int
  type Result2    = Long

  def parse(fileName: String): Parsed = {
    val lines = readFileLines(fileName)
    Field2D.parseFromLines(lines, _ - '0')
  }

  private def correctSlice(data: NonEmptyList[TreeHeight]): Boolean =
    data.tail.maxOption match {
      case None    => true
      case Some(m) => data.head > m
    }

  private def viewingDistance(value: NonEmptyList[TreeHeight]): Int = {
    val visibleTrees = value.tail.takeWhile(_ < value.head).size
    if (visibleTrees == value.tail.size) visibleTrees else visibleTrees + 1
  }

  private def slicesInAllDirections(
    data: Field2D[Int],
    from: Coords2D,
  ): List[NonEmptyList[TreeHeight]] = {
    val a = (0 to from.x.value).map(a => data.at(a, from.y.value)).reverse
    val b = (from.x.value until data.width) map { a =>
      data.at(a, from.y.value)
    }

    val c = (0 to from.y.value).map(a => data.at(from.x.value, a)).reverse
    val d = (from.y.value until data.height) map { a =>
      data.at(from.x.value, a)
    }

    (a :: b :: c :: d :: Nil).map(x => NonEmptyList.fromListUnsafe(x.toList))
  }

  def part1(data: Parsed): Result1 = {
    def isVisible(c: Coords2D): Boolean =
      slicesInAllDirections(data, c).exists(correctSlice)

    val visible = data.mapByCoords(isVisible)

    val visualisation: Field2D[Char] = visible.mapByValues {
      if (_) '█' else '░'
    }
    println(Field2D.debugPrint(visualisation))

    visible.count(_ == true)
  }

  def part2(data: Parsed): Result2 = {
    def scenicScore(c: Coords2D): Long =
      slicesInAllDirections(data, c).map(viewingDistance).product
    data.allCoords.map(scenicScore).max
  }

  def main(args: Array[String]): Unit = {
    correctSlice(NonEmptyList.of(1, 5, 5, 2)) shouldEqual false
    correctSlice(NonEmptyList.of(1, 2)) shouldEqual false
    correctSlice(NonEmptyList.of(1, 7)) shouldEqual false
    correctSlice(NonEmptyList.of(1, 3, 4, 9)) shouldEqual false

    viewingDistance(NonEmptyList.of(5, 3)) shouldEqual 1       // sees 3
    viewingDistance(NonEmptyList.of(5, 5, 2)) shouldEqual 1    // sees 5
    viewingDistance(NonEmptyList.of(5, 1, 2)) shouldEqual 2    // sees 1, 2
    viewingDistance(NonEmptyList.of(5, 3, 5, 3)) shouldEqual 2 // sees 3, 5

    val test = parse("2022/08-test.txt")
    val real = parse("2022/08.txt")

    part1(test) shouldEqual 21
    part1(real) shouldEqual 1690

    part2(test) shouldEqual 8
    part2(real) shouldEqual 535680
  }
}
