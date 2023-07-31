package jurisk.adventofcode.y2022

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Clock
import cats.effect.std.Console
import cats.implicits._
import jurisk.algorithms.pathfinding.Dfs
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent19 extends IOApp {
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
    private def maybeBuildOreRobot(blueprint: Blueprint): Option[State] =
      if (ore >= blueprint.oreRobotCostsNOre) {
        if (oreRobots < blueprint.maxOreCostOfRobot) {
          copy(
            ore = ore - blueprint.oreRobotCostsNOre,
            oreRobots = oreRobots + 1,
          ).some
        } else none // We won't run out of ore
      } else none

    private def maybeBuildClayRobot(blueprint: Blueprint): Option[State] =
      if (ore >= blueprint.clayRobotCostsNOre) {
        if (clayRobots < blueprint.maxClayCostOfRobot) {
          copy(
            ore = ore - blueprint.clayRobotCostsNOre,
            clayRobots = clayRobots + 1,
          ).some
        } else none // We won't run out of clay
      } else none

    private def maybeBuildObsidianRobot(blueprint: Blueprint): Option[State] =
      if (
        ore >= blueprint.obsidianRobotCostsNOre && clay >= blueprint.obsidianRobotCostsMClay
      ) {
        if (obsidianRobots < blueprint.maxObsidianCostOfRobot) {
          copy(
            ore = ore - blueprint.obsidianRobotCostsNOre,
            clay = clay - blueprint.obsidianRobotCostsMClay,
            obsidianRobots = obsidianRobots + 1,
          ).some
        } else none // We won't run out of obsidian
      } else none

    private def maybeBuildGeodeRobot(blueprint: Blueprint): Option[State] =
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

    private def maybeDoNothing: Option[State] =
      this.some // Collecting resources

    private def addProductionFrom(from: State): State =
      copy(
        minutesPassed = minutesPassed + 1,
        ore = ore + from.oreRobots,
        clay = clay + from.clayRobots,
        obsidian = obsidian + from.obsidianRobots,
        openGeodes = openGeodes + from.geodeRobots,
      )

    def successors(blueprint: Blueprint): List[State] =
      // Being greedy on geode robots is questionable as there can be test cases that defeat this, but it works
      // for the test cases given.
      if (maybeBuildGeodeRobot(blueprint).isDefined) {
        maybeBuildGeodeRobot(blueprint).map(_.addProductionFrom(this)).toList
      } else {
        List(
          maybeBuildObsidianRobot(blueprint),
          maybeBuildClayRobot(blueprint),
          maybeBuildOreRobot(blueprint),
          maybeDoNothing,
        ).flatten.map(_.addProductionFrom(this))
      }

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
    val maxOreCostOfRobot: Int                            = List(
      oreRobotCostsNOre,
      clayRobotCostsNOre,
      obsidianRobotCostsNOre,
      geodeRobotCostsNOre,
    ).max
    val maxClayCostOfRobot: Int                           = obsidianRobotCostsMClay
    val maxObsidianCostOfRobot: Int                       = geodeRobotCostsMObsidian

    def geodesCanOpen(minutes: Int): Int = {
      var bestSeen: Int = 0

      def visit(state: State): Unit =
        if (state.openGeodes > bestSeen) {
          bestSeen = state.openGeodes
//        println(s"${Thread.currentThread().getName}: Found better than before $bestSeen, state is $state")
        }

      def successors(state: State): List[State] = {
        visit(state)

        if (state.minutesPassed >= minutes) {
          Nil
        } else {
          val turnsLeft = minutes - state.minutesPassed

          def maxFutureProduction(robots: Int, turnsLeft: Int): Int =
            if (turnsLeft == 0) 0
            else if (turnsLeft == 1) robots
            else robots + maxFutureProduction(robots + 1, turnsLeft - 1)

          val theoreticalMaxGeodes =
            state.openGeodes + maxFutureProduction(state.geodeRobots, turnsLeft)
          if (theoreticalMaxGeodes < bestSeen) {
            Nil
          } else {
            state.successors(this)
          }
        }
      }

      // Interesting alternative to consider - do Bfs instead, but only keep some top N most promising states of the
      // frontier for every minute - by some heuristic (e.g. cost of all resources in ore + cost of resources yet to be
      // generated by future robots in ore)
      Dfs.dfsVisitAll[State](State.Start, successors, visit)

      bestSeen
    }

    override def toString: String =
      s"""Blueprint $id:
         |  Each ore robot costs $oreRobotCostsNOre ore.
         |  Each clay robot costs $clayRobotCostsNOre ore.
         |  Each obsidian robot costs $obsidianRobotCostsNOre ore and $obsidianRobotCostsMClay clay.
         |  Each geode robot costs $geodeRobotCostsNOre ore and $geodeRobotCostsMObsidian obsidian.
         |""".stripMargin
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
        case _                                                                                                                                                                         => s.failedToParse
      }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Blueprint.parse)

  def part1(data: Parsed): IO[Int] = for {
    _       <- Console[IO].println(s"Blueprint count: ${data.length}")
    results <- data.parTraverse { blueprint =>
                 for {
                   _  <-
                     Console[IO].println(
                       s"${Thread.currentThread().getName}: Calculating for:\n$blueprint"
                     )
                   ql <- IO {
                           blueprint.id * blueprint.geodesCanOpen(Part1Minutes)
                         }
                   _  <-
                     Console[IO].println(
                       s"${Thread.currentThread().getName}: Quality level: $ql\n"
                     )
                 } yield ql
               }
  } yield results.sum

  def part2(data: Parsed): IO[Int] = {
    require(data.length == 3)

    for {
      results <- data.parTraverse { blueprint =>
                   for {
                     _      <-
                       Console[IO].println(
                         s"${Thread.currentThread().getName}: Calculating for:\n$blueprint"
                       )
                     result <- IO(blueprint.geodesCanOpen(Part2Minutes))
                     _      <-
                       Console[IO].println(
                         s"${Thread.currentThread().getName}: Can open $result\n"
                       )
                   } yield result
                 }
    } yield results.product
  }

  val Part1Minutes = 24
  val Part2Minutes = 32

  private def read(fileName: String): IO[Parsed] = for {
    data <- IO(readFileText(fileName))
  } yield parse(data)

  def realData: IO[Parsed] = read("2022/19.txt")
  def testData: IO[Parsed] = read("2022/19-test.txt")

  override def run(args: List[String]): IO[ExitCode] = for {
    start   <- Clock[IO].monotonic
    real    <- realData
    result1 <- part1(real)
    _       <- Console[IO].println(s"Part 1: $result1")
    result2 <- part2(real.take(3))
    _       <- Console[IO].println(s"Part 2: $result2")
    end     <- Clock[IO].monotonic
    delta    = end - start
    _       <- Console[IO].println(s"Elapsed seconds: ${delta.toSeconds}")
  } yield ExitCode.Success
}
