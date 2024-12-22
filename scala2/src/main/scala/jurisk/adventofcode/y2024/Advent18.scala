package jurisk.adventofcode.y2024

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import jurisk.algorithms.pathfinding.AStar
import jurisk.collections.immutable.ImmutableBitSet
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.ToInt

import scala.annotation.tailrec

object Advent18 {
  type Input = List[Coords2D]
  type N     = Int

  def parse(input: String): Input =
    input.parseLines(Coords2D.parse)

  def solution(
    set: ImmutableBitSet[Coords2D],
    start: Coords2D,
    end: Coords2D,
  ): Option[(NonEmptyList[Coords2D], N)] = {
    val area                                           = Area2D(start, end)
    def neighbours(c: Coords2D): List[(Coords2D, Int)] =
      c.adjacent4
        .filter { c =>
          !set.contains(c) && area.contains(c)
        }
        .map((_, 1))
    def heuristic(c: Coords2D): Int                    =
      c.manhattanDistance(end)
    AStar.aStar[Coords2D, Int](start, neighbours, heuristic, _ == end).map {
      case (path, distance) => (path, distance)
    }
  }

  def part1(data: Input, take: Int, start: Coords2D, end: Coords2D): N = {
    val area                            = Area2D(start, end)
    implicit val toInt: ToInt[Coords2D] = area.coordsToInt

    val busy = ImmutableBitSet.fromSpecific(data.take(take))
    solution(busy, start, end)
      .map { case (_, distance) => distance }
      .getOrElse("No solution".fail)
  }

  def part2(data: Input, take: Int, start: Coords2D, end: Coords2D): String = {
    val area                            = Area2D(start, end)
    implicit val toInt: ToInt[Coords2D] = area.coordsToInt

    val busy        = ImmutableBitSet.fromSpecific(data.take(take))
    val initialPath = solution(busy, start, end)
      .map { case (path, _) => path }
      .getOrElse("No initial solution".fail)

    @tailrec
    def updatePath(
      busy: ImmutableBitSet[Coords2D],
      path: NonEmptyList[Coords2D],
      remaining: List[Coords2D],
    ): Coords2D =
      remaining match {
        case Nil    => "No solution".fail
        case h :: t =>
          val newBusy = busy + h
          if (path.iterator.contains(h)) {
            solution(newBusy, start, end) match {
              case Some((newPath, _)) => updatePath(newBusy, newPath, t)
              case None               => h
            }
          } else {
            updatePath(newBusy, path, t)
          }
      }

    val c = updatePath(busy, initialPath, data.drop(take))
    s"${c.x},${c.y}"
  }

  val RealEnd: Coords2D = Coords2D(70, 70)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/18$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 1024, Coords2D.Zero, RealEnd)}")
    println(s"Part 2: ${part2(realData, 1024, Coords2D.Zero, RealEnd)}")
  }
}
