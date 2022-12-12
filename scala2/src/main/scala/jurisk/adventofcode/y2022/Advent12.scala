package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.algorithms.pathfinding.Pathfinding
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.Parsing.{PrefixRemover, StringOps}
import jurisk.utils.Utils.IterableOps
import jurisk.utils.FileInput._
import org.scalatest.matchers.should.Matchers._

object Advent12 {
  type Elevation = Char
  type Parsed = (Field2D[Elevation], Coords2D, Coords2D)

  def parse(data: String): Parsed = {
    val charField = Field2D.parseFromString(data, identity)
//    val start = charField.findFirst('S')
//    val end = charField.findFirst('E')
    var start: Coords2D = null
    var end: Coords2D = null
    val field = charField.map { case (coords, ch) =>
      ch match {
        case 'S' =>
          start = coords
          'a'

        case 'E' =>
          end = coords
          'z'

        case ch => ch
      }
    }

    (field, start, end)
  }

  def part1(data: Parsed): Option[Long] = {
    val (field, start, end) = data

    def successors(c: Coords2D): List[Coords2D] = {
      val result = c.neighbours(includeDiagonal = false).filter { n =>
        val thisElev = field.atUnsafe(c)
        val otherElev = field.at(n)
        otherElev match {
          case None => false
          case Some(otherElev) =>         (otherElev.toInt - thisElev.toInt) <= 1
        }
      }

      // println(s"$c => $result")
      result
    }

    val path = Pathfinding.bfs[Coords2D](start, successors, _ == end)
    // println(path)
    path.map(_.size - 1)
  }

  def part2(data: Parsed): Long = {
    val (field, start, end) = data

    val lowestCoords = field.allCoords.filter { c =>
      field.atUnsafe(c) == 'a'
    }

    println(lowestCoords.size)

    lowestCoords.flatMap(c => part1(field, c, end)).min
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/12-test.txt")
    val realData = readFileText("2022/12.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual Some(31)
    part1(real) shouldEqual Some(412)

    part2(test) shouldEqual 29
    part2(real) shouldEqual 402
  }
}
