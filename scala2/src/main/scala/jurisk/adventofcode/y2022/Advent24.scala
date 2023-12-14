package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.algorithms.pathfinding.Pathfinding
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D
import jurisk.math.absForWrappingAround
import jurisk.math.lcm
import jurisk.utils.FileInput._
import jurisk.utils.Memoize.memoize1
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent24 {
  final case class Blizzard(
    startsAt: Coords2D,
    direction: CardinalDirection2D,
  )

  final case class Maze(
    start: Coords2D,
    blizzards: Set[Blizzard],
    goal: Coords2D,
    area: Area2D,
  ) {
    def freeAtTimeNear(time: Int, location: Coords2D): List[Coords2D] = {
      val blizzardCoordinatesInNext = blizzardsAt(time)
      (location :: location.adjacent4)
        .filterNot { c =>
          isWall(c) || blizzardCoordinatesInNext.contains(c)
        }
    }

    private val cycleTime: Int = lcm(area.width - 2, area.height - 2)

    def nextTimeWrapping(time: Int): Int = (time + 1) % cycleTime

    private def isWall(c: Coords2D): Boolean =
      (c != start) && (c != goal) && (area
        .onInsideEdge(c) || (!area.contains(c)))

    def blizzardAt(blizzard: Blizzard, time: Int): Coords2D =
      if (blizzard.direction.isVertical) {
        val newY = absForWrappingAround(
          (blizzard.startsAt.y - 1) + (blizzard.direction.diff * time).y,
          area.height - 2,
        ) + 1
        Coords2D.of(
          blizzard.startsAt.x,
          newY,
        )
      } else { // horizontal
        val newX = absForWrappingAround(
          (blizzard.startsAt.x - 1) + (blizzard.direction.diff * time).x,
          area.width - 2,
        ) + 1
        Coords2D.of(
          newX,
          blizzard.startsAt.y,
        )
      }

    private def calculateBlizzardsAtTime(time: Int): Set[Coords2D] =
      blizzards.map(b => blizzardAt(b, time))
    private val blizzardsAt: Int => Set[Coords2D]                  = memoize1(
      calculateBlizzardsAtTime
    )
  }

  sealed trait Command
  object Maze {
    def parse(s: String): Maze = {
      val charField = Field2D.parseCharField(s)

      val start = Coords2D.of(charField.firstRowValues.indexOf('.'), 0)
      val goal  =
        Coords2D.of(charField.lastRowValues.indexOf('.'), charField.height - 1)

      val blizzards = charField.entries.flatMap { case (c, ch) =>
        val direction = Direction2D.parseCaretToOption(ch)
        direction map { direction =>
          Blizzard(c, direction)
        }
      }.toSet

      Maze(
        start = start,
        blizzards,
        goal = goal,
        area = Area2D(charField.topLeft, charField.bottomRight),
      )
    }
  }

  sealed trait State[T <: State[T]] {
    def isGoal(maze: Maze): Boolean
    def successors(maze: Maze): List[T]
  }

  final case class State1(
    time: Int,
    location: Coords2D,
  ) extends State[State1] {
    def isGoal(maze: Maze): Boolean = location == maze.goal

    def successors(maze: Maze): List[State1] = {
      val nextTime = maze.nextTimeWrapping(time)
      val free     = maze.freeAtTimeNear(nextTime, location)

      free map { coords =>
        State1(nextTime, coords)
      }
    }
  }

  object State1 {
    def start(maze: Maze): State1 = State1(0, maze.start)
  }

  final case class State2(
    time: Int,
    location: Coords2D,
    goalVisited: Boolean,
    snacksCollected: Boolean,
  ) extends State[State2] {
    def isGoal(maze: Maze): Boolean =
      goalVisited && snacksCollected && location == maze.goal

    def successors(maze: Maze): List[State2] = {
      val nextTime = maze.nextTimeWrapping(time)
      val free     = maze.freeAtTimeNear(nextTime, location)

      free map { coords =>
        State2(
          nextTime,
          coords,
          goalVisited = goalVisited || coords == maze.goal,
          snacksCollected =
            snacksCollected || (goalVisited && coords == maze.start),
        )
      }
    }
  }

  object State2 {
    def start(maze: Maze): State2 =
      State2(0, maze.start, goalVisited = false, snacksCollected = false)
  }

  private def debugPrint(state: State1, maze: Maze): Unit = {
    val field: Field2D[List[CardinalDirection2D]] =
      Field2D.forArea(maze.area, Nil)

    val withBlizzards = maze.blizzards.foldLeft(field) { case (acc, b) =>
      val location = maze.blizzardAt(b, state.time)
      val oldList  = acc.atOrElse(location, Nil)
      val newList  = b.direction :: oldList
      acc.updatedAtUnsafe(location, newList)
    }

    withBlizzards.atOrElse(state.location, Nil) shouldEqual Nil

    val charField = withBlizzards.map {
      case Nil                    => '.'
      case x :: Nil               => x.toCaret
      case list if list.size < 10 => list.size.toString.head
      case _                      => 'N'
    }

    val result = charField.updatedAtUnsafe(state.location, 'E')

    Field2D.printField[Char](s"${state.time} time:".some, result, identity)
  }

  private def solve[T <: State[T]](maze: Maze, startF: Maze => T): Int = {
    val cost = Pathfinding
      .shortestPathLength[T](startF(maze), _.successors(maze), _.isGoal(maze))
      .getOrElse("Path not found".fail)
    println(s"Found $cost")
    cost
  }

  def part1(maze: Maze): Int = solve(maze, State1.start)
  def part2(maze: Maze): Int = solve(maze, State2.start)

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/24-test.txt")
    val realData = readFileText("2022/24.txt")

    val test = Maze.parse(testData)
    val real = Maze.parse(realData)

    part1(test) shouldEqual 18
    part1(real) shouldEqual 281

    part2(test) shouldEqual 54
    part2(real) shouldEqual 807
  }
}
