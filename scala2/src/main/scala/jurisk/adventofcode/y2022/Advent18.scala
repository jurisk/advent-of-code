package jurisk.adventofcode.y2022

import jurisk.algorithms.pathfinding.Bfs
import jurisk.geometry.Coords3D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent18 {
  private def sidesFree(cube: Coords3D[Int], cubes: Set[Coords3D[Int]]): Int =
    cube.adjacent6.count(n => !cubes.contains(n))

  def parse(data: String): Set[Coords3D[Int]] =
    data.parseLines(Coords3D.parse[Int]).toSet

  def part1(points: Set[Coords3D[Int]]): Int =
    points.toList.map(sidesFree(_, points)).sum

  def part2(points: Set[Coords3D[Int]]): Int = {
    val boundingBox         = Coords3D.boundingBoxInclusive(points)
    val expandedBoundingBox = boundingBox.expandInEachDirectionBy(1)

    val startingPointForSearch = expandedBoundingBox.min
    val reachable              = Bfs
      .bfsReachable[Coords3D[Int]](
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
