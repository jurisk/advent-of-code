package jurisk.adventofcode.y2022

import jurisk.algorithms.pathfinding.Bfs
import jurisk.geometry.{Area3D, Coords3D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent18 {
  private def sidesFree(cube: Coords3D, cubes: Set[Coords3D]): Int =
    cube.adjacent6.count(n => !cubes.contains(n))

  def parse(data: String): Set[Coords3D] =
    data.parseList("\n", Coords3D.parse).toSet

  def part1(points: Set[Coords3D]): Int =
    points.toList.map(sidesFree(_, points)).sum

  def part2(points: Set[Coords3D]): Int = {
    val boundingBox         = Coords3D.boundingBoxInclusive(points)
    val expandedBoundingBox = Area3D(
      boundingBox.min - Coords3D(1, 1, 1),
      boundingBox.max + Coords3D(1, 1, 1),
    )

    val startingPointForSearch = expandedBoundingBox.min
    val reachable              = Bfs
      .bfsReachable[Coords3D](
        startingPointForSearch,
        _.adjacent6.filter { n =>
          !points.contains(n) && expandedBoundingBox.contains(n)
        },
      )
      .toSet

    val nonReachable = boundingBox.points.toSet -- reachable

    part1(nonReachable)
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/18-test.txt")
    val realData = readFileText("2022/18.txt")

    val smallTest = parse("1,1,1\n2,1,1")
    val test      = parse(testData)
    val real      = parse(realData)

    part1(smallTest) shouldEqual 10
    part1(test) shouldEqual 64
    part1(real) shouldEqual 4320

    part2(test) shouldEqual 58
    part2(real) shouldEqual 2456
  }
}
