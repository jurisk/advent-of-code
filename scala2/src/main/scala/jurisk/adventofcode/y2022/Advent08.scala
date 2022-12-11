package jurisk.adventofcode.y2022

import cats.data.NonEmptyList
import jurisk.utils.FileInput._
import jurisk.geometry.{Coords2D, Field2D}
import org.scalatest.matchers.should.Matchers._

object Advent08 {
  type TreeHeight = Int
  type Parsed     = Field2D[TreeHeight]
  type Processed  = Parsed
  type Result1    = Int
  type Result2    = Long

  def readFileAndParse(fileName: String): Parsed = {
    val lines = readFileLines(fileName)
    Field2D.parseFromLines(lines, _ - '0')
  }

  private def visibleFromOutside(data: NonEmptyList[TreeHeight]): Boolean =
    data.tail.maxOption match {
      case None    => true
      case Some(m) => data.head > m
    }

  private def viewingDistance(value: NonEmptyList[TreeHeight]): Int = {
    val visibleTrees = value.tail.takeWhile(_ < value.head).size
    if (visibleTrees == value.tail.size) visibleTrees else visibleTrees + 1
  }

  private def slicesInAllDirections[T](
    data: Field2D[T],
    from: Coords2D,
  ): List[NonEmptyList[T]] = {
    def splitBothDirections(
      value: Vector[T],
      idx: Int,
    ): List[NonEmptyList[T]] =
      NonEmptyList.fromListUnsafe(value.toList.take(idx + 1).reverse) ::
        NonEmptyList.fromListUnsafe(value.toList.drop(idx)) ::
        Nil

    splitBothDirections(
      data.column(from.x),
      from.y.value,
    ) ::: splitBothDirections(data.row(from.y), from.x.value)
  }

  def part1(data: Parsed): Result1 = {
    def isVisible(c: Coords2D): Boolean =
      slicesInAllDirections(data, c).exists(visibleFromOutside)

    val visible = data.mapByCoords(isVisible)

    val visualisation: Field2D[Char] = visible.mapByValues {
      if (_) '█' else '░'
    }
    println(Field2D.toDebugRepresentation(visualisation))

    visible.count(_ == true)
  }

  def part2(data: Parsed): Result2 = {
    def scenicScore(c: Coords2D): Long =
      slicesInAllDirections(data, c).map(viewingDistance).product
    data.allCoords.map(scenicScore).max
  }

  def main(args: Array[String]): Unit = {
    visibleFromOutside(NonEmptyList.of(1, 5, 5, 2)) shouldEqual false
    visibleFromOutside(NonEmptyList.of(1, 2)) shouldEqual false
    visibleFromOutside(NonEmptyList.of(1, 7)) shouldEqual false
    visibleFromOutside(NonEmptyList.of(1, 3, 4, 9)) shouldEqual false

    viewingDistance(NonEmptyList.of(5, 3)) shouldEqual 1       // sees 3
    viewingDistance(NonEmptyList.of(5, 5, 2)) shouldEqual 1    // sees 5
    viewingDistance(NonEmptyList.of(5, 1, 2)) shouldEqual 2    // sees 1, 2
    viewingDistance(NonEmptyList.of(5, 3, 5, 3)) shouldEqual 2 // sees 3, 5

    val test = readFileAndParse("2022/08-test.txt")
    val real = readFileAndParse("2022/08.txt")

    part1(test) shouldEqual 21
    part1(real) shouldEqual 1690

    part2(test) shouldEqual 8
    part2(real) shouldEqual 535680
  }
}
