package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.algorithms.pathfinding.Dfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent19 {
  type Parsed = List[Blueprint]

  final case class State(
    minutesPassed: Int = 0,
    oreRobots: Int = 0,
    clayRobots: Int = 0,
    obsidianRobots: Int = 0,
    geodeRobots: Int = 0,
    ore: Int = 0,
    clay: Int = 0,
    obsidian: Int = 0,
    openGeodes: Int = 0,
  ) {
    def maybeBuildOreRobot(blueprint: Blueprint): Option[State] =
      if (ore >= blueprint.oreRobotCostsNOre) {
        copy(
          ore = ore - blueprint.oreRobotCostsNOre,
          oreRobots = oreRobots + 1,
        ).some
      } else {
        none
      }

    def maybeBuildClayRobot(blueprint: Blueprint): Option[State] =
      if (ore >= blueprint.clayRobotCostsNOre) {
        copy(
          ore = ore - blueprint.clayRobotCostsNOre,
          clayRobots = clayRobots + 1,
        ).some
      } else {
        none
      }

    def maybeBuildObsidianRobot(blueprint: Blueprint): Option[State] =
      if (
        ore >= blueprint.obsidianRobotCostsNOre && clay >= blueprint.obsidianRobotCostsMClay
      ) {
        copy(
          ore = ore - blueprint.obsidianRobotCostsNOre,
          clay = clay - blueprint.obsidianRobotCostsMClay,
          obsidianRobots = obsidianRobots + 1,
        ).some
      } else {
        none
      }

    def maybeBuildGeodeRobot(blueprint: Blueprint): Option[State] =
      if (
        ore >= blueprint.geodeRobotCostsNOre && obsidian >= blueprint.geodeRobotCostsMObsidian
      ) {
        copy(
          ore = ore - blueprint.geodeRobotCostsNOre,
          obsidian = obsidian - blueprint.geodeRobotCostsMObsidian,
          geodeRobots = geodeRobots + 1,
        ).some
      } else {
        none
      }

    def maybeDoNothing(blueprint: Blueprint): Option[State] =
      this.some // Collecting resources

    def addProductionFrom(from: State): State =
      copy(
        minutesPassed = minutesPassed + 1,
        ore = ore + from.oreRobots,
        clay = clay + from.clayRobots,
        obsidian = obsidian + from.obsidianRobots,
        openGeodes = openGeodes + from.geodeRobots,
      )

    def successors(blueprint: Blueprint): List[State] =
      // TODO: this builds only 1 robot! Not enough! Do we need combinations?
      List(
        maybeBuildGeodeRobot(blueprint),
        maybeBuildObsidianRobot(blueprint),
        maybeBuildClayRobot(blueprint),
        maybeBuildOreRobot(blueprint),
        maybeDoNothing(blueprint),
      ).flatten.map(_.addProductionFrom(this))

    override def toString: String =
      s"Minutes = $minutesPassed | Robots ore = $oreRobots clay = $clayRobots obsidian = $obsidianRobots geode = $geodeRobots | Resources ore = $ore clay = $clay obsidian = $obsidian geodes = $openGeodes"
  }

  object State {
    val Start: State = State(0, 1, 0, 0, 0, 0, 0, 0, 0)
  }

  final case class Blueprint(
    id: Int,
    oreRobotCostsNOre: Int,
    clayRobotCostsNOre: Int,
    obsidianRobotCostsNOreAndMClay: (Int, Int),
    geodeRobotCostsNOreAndMObsidian: (Int, Int),
  ) {
    val (obsidianRobotCostsNOre, obsidianRobotCostsMClay) =
      obsidianRobotCostsNOreAndMClay
    val (geodeRobotCostsNOre, geodeRobotCostsMObsidian)   =
      geodeRobotCostsNOreAndMObsidian

    def geodesCanOpen(minutes: Int): Int = {
      var bestSeen: Int = 0

      def visit(state: State): Unit =
        // println(state)

        if (state.openGeodes > bestSeen) {
          bestSeen = state.openGeodes
          println(s"Found better than before $bestSeen, state is $state")
        }

      def successors(state: State): List[State] =
        if (state.minutesPassed >= minutes) {
          Nil
        } else {
          state.successors(this)
        }

      Dfs.dfsVisitAll[State](State.Start, successors, visit)

      bestSeen
    }

    def qualityLevel: Int =
      id * geodesCanOpen(24)
  }

  object Blueprint {
    def parse(s: String): Blueprint =
      s match {
        case s"Blueprint $id: Each ore robot costs $a ore. Each clay robot costs $b ore. Each obsidian robot costs $c ore and $d clay. Each geode robot costs $e ore and $f obsidian." =>
          Blueprint(
            id.toInt,
            a.toInt,
            b.toInt,
            (c.toInt, d.toInt),
            (e.toInt, f.toInt),
          )
        case _                                                                                                                                                                         => s"Failed to parse $s".fail
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Blueprint.parse)

  def part1(data: Parsed): Int = {
    println(s"Blueprint count: ${data.length}")
    data.map { blueprint =>
      println(s"Calculating for: $blueprint")
      val ql = blueprint.qualityLevel
      println(s"Quality level: $ql")
      println
      ql
    }.sum
  }

  def part2(data: Parsed): String =
    data.counts.toString

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/19-test.txt")
    val realData = readFileText("2022/19.txt")

    val test = parse(testData)
    val real = parse(realData)

    val test1 = test.head
    State.Start shouldEqual State(oreRobots = 1)
    val m1    = State(minutesPassed = 1, oreRobots = 1, ore = 1)
    State.Start.successors(test1) shouldEqual m1 :: Nil
    val m2    = State(minutesPassed = 2, oreRobots = 1, ore = 2)
    m1.successors(test1) shouldEqual m2 :: Nil

    test1.geodesCanOpen(24) shouldEqual 9

    val test2 = test(1)
    test2.geodesCanOpen(24) shouldEqual 12

//    part1(test) shouldEqual 33

//    part1(real) shouldEqual 12345678
//
//    part2(test) shouldEqual "asdf"
//    part2(real) shouldEqual "asdf"
  }
}
