package jurisk.adventofcode.y2023

import jurisk.geometry.{Area2D, Area3D, Coords2D, Coords3D, Field2D}
import jurisk.utils.CollectionOps.VectorOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent22 {
  type Input   = Vector[Brick]
  type BrickId = Int

  final case class Brick(id: BrickId, blocks: Area3D) {
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

  def part1(data: Input): Int = {
    println(s"${data.length} blocks")

    data foreach println
    println()
    val landed = landBricks(data)
    landed foreach println
    println()

    landed.zipWithIndex count { case (brick, index) =>
      print(s"Considering $brick...")

      val withoutThis = landed.removeAt(index)
      val packedAgain = landBricks(withoutThis)
      val result      = withoutThis.toSet == packedAgain.toSet
      println(result)
      result
    }
  }

  private def landBricks(data: Input): Input = {
    var landed: Vector[Brick] = Vector.empty
    val sorted                = data.sortBy(_.blocks.min.z)

    val allBlocks = data.map(_.blocks)
    val allX      = allBlocks.map(_.min.x) ++ allBlocks.map(_.max.x)
    val allY      = allBlocks.map(_.min.y) ++ allBlocks.map(_.max.y)
    val minX      = allX.min
    val maxX      = allX.max

    val minY = allY.min
    val maxY = allY.max

    assert(minX == 0)
    assert(minY == 0)

    var bottom =
      Field2D.forArea(Area2D(Coords2D(minX, minY), Coords2D(maxX, maxY)), 0)

    sorted foreach { brick =>
      val zDiffs = brick.blocks.points.map { point =>
        val bottomLevel = bottom.atOrElse(Coords2D(point.x, point.y), 0)
        val possibleZ   = bottomLevel + 1
        point.z - possibleZ
      }

      assert(zDiffs.forall(_ >= 0))
      val canFall  = zDiffs.min
      val adjusted =
        brick.copy(blocks = brick.blocks moveBy Coords3D(0, 0, -canFall))

      adjusted.blocks.points foreach { adjustedPoint =>
        bottom = bottom.modifyUnsafe(
          Coords2D(adjustedPoint.x, adjustedPoint.y),
          oldZ => oldZ max adjustedPoint.z,
        )
      }

      landed = adjusted +: landed
    }

    landed.reverse
  }

  def part2(data: Input): Int = {
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

      (withoutThis.sortBy(_.id) zip packedAgain.sortBy(_.id)).count {
        case (a, b) =>
          a != b
      }
    }.sum
  }

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
