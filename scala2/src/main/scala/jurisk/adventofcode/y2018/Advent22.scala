package jurisk.adventofcode.y2018

import jurisk.adventofcode.y2018.Advent22.RegionType.Impassable
import jurisk.adventofcode.y2018.Advent22.RegionType.Narrow
import jurisk.adventofcode.y2018.Advent22.RegionType.Rocky
import jurisk.adventofcode.y2018.Advent22.RegionType.Wet
import jurisk.adventofcode.y2018.Advent22.Tool.ClimbingGear
import jurisk.adventofcode.y2018.Advent22.Tool.Neither
import jurisk.adventofcode.y2018.Advent22.Tool.Torch
import jurisk.algorithms.pathfinding.AStar
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.utils.Memoize.memoize
import jurisk.utils.Parsing.StringOps
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent22 {
  sealed trait RegionType {
    def riskLevel: Int
    def validTools: Set[Tool]
  }
  object RegionType       {
    case object Rocky      extends RegionType {
      override def riskLevel: Int        = 0
      override def validTools: Set[Tool] = Set(ClimbingGear, Torch)
    }
    case object Narrow     extends RegionType {
      override def riskLevel: Int        = 2
      override def validTools: Set[Tool] = Set(Torch, Neither)
    }
    case object Wet        extends RegionType {
      override def riskLevel: Int        = 1
      override def validTools: Set[Tool] = Set(ClimbingGear, Neither)
    }
    case object Impassable extends RegionType {
      override def riskLevel: Int        = "Should not be called".fail
      override def validTools: Set[Tool] = Set.empty
    }
  }

  final case class Input(depth: Int, target: Coords2D) {
    private lazy val erosionLevel: Coords2D => Int = memoize(
      calculateErosionLevel
    )

    lazy val regionType: Coords2D => RegionType = memoize(calculateRegionType)

    private def calculateRegionType(c: Coords2D): RegionType =
      if ((c.x < 0) || (c.y < 0)) {
        Impassable
      } else {
        erosionLevel(c) % 3 match {
          case 0 => Rocky
          case 1 => Wet
          case 2 => Narrow
          case _ => "Should not happen".fail
        }
      }

    private def calculateErosionLevel(c: Coords2D): Int =
      (geologicIndex(c) + depth) % 20183

    private def geologicIndex(c: Coords2D): Int =
      if (c == Coords2D.Zero) 0
      else if (c == target) 0
      else if (c.y == 0) {
        c.x * 16807
      } else if (c.x == 0) {
        c.y * 48271
      } else if ((c.x < 0) || (c.y < 0)) {
        "Should not be traversed".fail
      } else {
        erosionLevel(c.W) * erosionLevel(c.N)
      }

    def riskLevel(c: Coords2D): Int =
      regionType(c).riskLevel
  }

  sealed trait Tool
  object Tool {
    case object ClimbingGear extends Tool
    case object Torch        extends Tool
    case object Neither      extends Tool
  }

  final case class State(location: Coords2D, toolEquipped: Tool)

  def part1(input: Input): Int = {
    val result =
      Area2D(Coords2D.Zero, input.target).points.map(input.riskLevel).sum
    println(result)
    result
  }

  def part2(input: Input): Int = {
    def successors(state: State): List[(State, Int)] = {
      val otherTool       = (input
        .regionType(state.location)
        .validTools - state.toolEquipped).singleElementUnsafe
      val validNeighbours = state.location.adjacent4.filter(c =>
        input.regionType(c).validTools.contains(state.toolEquipped)
      )
      (state.copy(toolEquipped = otherTool), 7) :: validNeighbours.map(c =>
        (state.copy(location = c), 1)
      )
    }

    val target = State(input.target, Torch)

    // The heuristic always has to be shorter or equal than the real path length (admissible - never overestimates the
    // cost of reaching the goal).
    def heuristic(state: State): Int =
      state.location manhattanDistance input.target

    val (_, cost) = AStar
      .aStar[State, Int](
        State(Coords2D.Zero, Torch),
        successors,
        heuristic,
        _ == target,
      )
      .getOrElse("Did not find".fail)
    println(cost)
    cost
  }

  def main(args: Array[String]): Unit = {
    val testInput  = Input(510, Coords2D.of(10, 10))
    val testInput2 = Input(3198, Coords2D.of(12, 757))
    val realInput  = Input(6969, Coords2D.of(9, 796))

    part1(testInput) shouldEqual 114
    part1(testInput2) shouldEqual 9659
    part1(realInput) shouldEqual 7901

    part2(testInput) shouldEqual 45
    part2(testInput2) shouldEqual 1043
    part2(realInput) shouldEqual 1087
  }
}
