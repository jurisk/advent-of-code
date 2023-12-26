package jurisk.adventofcode.y2023

import jurisk.geometry.Area2D
import jurisk.geometry.Area3D
import jurisk.geometry.Coords2D
import jurisk.geometry.Coords3D
import jurisk.geometry.Field2D
import jurisk.utils.CollectionOps.VectorOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent22 {
  type Input   = Vector[Brick]
  type BrickId = Int

  final case class Brick(id: BrickId, blocks: Area3D[Int]) {
    def name: String = if (id < 26) {
      ('A'.toInt + id).toChar.toString
    } else {
      s"B$id"
    }

    override def toString: String =
      s"$name: ${blocks.min} -> ${blocks.max}"
  }

  private def parseBrick(s: String, id: BrickId): Brick =
    s match {
      case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
        Brick(
          id,
          Area3D(
            min = Coords3D(
              x1.toInt min x2.toInt,
              y1.toInt min y2.toInt,
              z1.toInt min z2.toInt,
            ),
            max = Coords3D(
              x1.toInt max x2.toInt,
              y1.toInt max y2.toInt,
              z1.toInt max z2.toInt,
            ),
          ),
        )
      case _                          => s.failedToParse
    }

  def parse(input: String): Input =
    input.splitLines.toVector.zipWithIndex.map { case (s, idx) =>
      parseBrick(s, idx)
    }

  private def landBricks(data: Input): Input = {
    val bottomArea = {
      val allBlocks = data.map(_.blocks)
      val allX      = allBlocks.map(_.min.x) ++ allBlocks.map(_.max.x)
      val allY      = allBlocks.map(_.min.y) ++ allBlocks.map(_.max.y)
      val minX      = allX.min
      val maxX      = allX.max

      val minY = allY.min
      val maxY = allY.max

      assert(minX == 0)
      assert(minY == 0)

      Area2D(Coords2D(minX, minY), Coords2D(maxX, maxY))
    }

    var landed: Vector[Brick] = Vector.empty
    var bottom                =
      Field2D.forArea(bottomArea, 0)

    val sorted = data.sortBy(_.blocks.min.z)
    sorted foreach { brick =>
      val canFall = {
        val zDiffs = brick.blocks.points.map { point =>
          val bottomLevel = bottom.atOrElse(Coords2D(point.x, point.y), 0)
          val possibleZ   = bottomLevel + 1
          point.z - possibleZ
        }

        zDiffs.min
      }

      assert(canFall >= 0)
      val adjusted =
        brick.copy(blocks = brick.blocks moveBy Coords3D(0, 0, -canFall))

      adjusted.blocks.points foreach { adjustedPoint =>
        bottom = bottom.modifyUnsafe(
          Coords2D(adjustedPoint.x, adjustedPoint.y),
          oldZ => oldZ max adjustedPoint.z,
        )
      }

      landed = landed :+ adjusted
    }

    landed
  }

  def solve(data: Input): Vector[Int] = {
    println(s"${data.length} blocks")

    data foreach println
    println()
    val landed = landBricks(data)
    landed foreach println
    println()

    landed.zipWithIndex.map { case (brick, index) =>
      print(s"Considering $brick...")

      val withoutThis = landed.removeAt(index)
      val packedAgain = landBricks(withoutThis)

      val result =
        (withoutThis.sortBy(_.id) zip packedAgain.sortBy(_.id)).count {
          case (a, b) =>
            a != b
        }

      println(s"causes $result to fall")

      result
    }
  }

  def part1(data: Input): Int =
    solve(data) count { _ == 0 }

  def part2(data: Input): Int =
    solve(data).sum

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/22$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
