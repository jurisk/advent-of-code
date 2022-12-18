package jurisk.adventofcode.y2022

import jurisk.algorithms.pathfinding.{AStar, Bfs}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent18 {
  final case class Coords3D(x: Int, y: Int, z: Int) {
    def neighbours6: List[Coords3D] = List(
      Coords3D(x + 1, y, z),
      Coords3D(x - 1, y, z),
      Coords3D(x, y + 1, z),
      Coords3D(x, y - 1, z),
      Coords3D(x, y, z + 1),
      Coords3D(x, y, z - 1),
    )

    def sidesFree(cubes: Set[Coords3D]): Int = {
      val result = neighbours6.count(n => !cubes.contains(n))
      println(s"$this $result")
      result
    }

    def manhattanDistance(other: Coords3D): Int =
      Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
  }

  type Parsed = Set[Coords3D]

  object Coords3D {
    def parse(s: String): Coords3D =
      s match {
        case s"$x,$y,$z" => Coords3D(x.toInt, y.toInt, z.toInt)
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Coords3D.parse).toSet

  def part1(data: Parsed): Int =
    data.toList.map(_.sidesFree(data)).sum

  def isReachableFromOutside(
    outside: Coords3D,
    c: Coords3D,
    parsed: Set[Coords3D],
  ): Boolean =
    AStar
      .aStar[Coords3D, Int](
        outside,
        x => x.neighbours6.filter(q => !parsed.contains(q)).map(q => (q, 1)),
        x => x.manhattanDistance(c),
        _ == c,
      )
      .isDefined

  def part2(data: Parsed): Int = {
    val minX = data.map(_.x).min
    val minY = data.map(_.y).min
    val minZ = data.map(_.z).min
    val maxX = data.map(_.x).max
    val maxY = data.map(_.y).max
    val maxZ = data.map(_.z).max

    val outside = Coords3D(minX - 1, minY - 1, minZ - 1)

    val reachable = Bfs
      .bfsReachable[Coords3D](
        outside,
        c =>
          c.neighbours6.filter { n =>
            !data.contains(
              n
            ) && n.x >= minX - 1 && n.x <= maxX + 1 && n.y >= minY - 1 && n.y <= maxY + 1 && n.z >= minZ - 1 && n.z <= maxZ + 1
          },
      )
      .toSet

    var points = data
    (minX to maxX).foreach { x =>
      (minY to maxY).foreach { y =>
        (minZ to maxZ).foreach { z =>
          val c = Coords3D(x, y, z)
          if (!reachable.contains(c)) {
            points = points + c
          }
        }
      }
    }

    part1(points)
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
