package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.utils.Utils.IterableOps
import jurisk.utils.FileInput.parseFileLines
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent07 {
  final case class Step(value: Char) extends AnyVal {
    def duration(additionalCost: Int): Int = additionalCost + (value - 'A') + 1
    override def toString: String          = value.toString
  }

  implicit val stepOrdering: Ordering[Step] = Ordering[Char].contramap(_.value)

  final case class Requirement(step: Step, requires: Step)

  object Requirement {
    private val RegEx                 =
      """Step ([A-Z]) must be finished before step ([A-Z]) can begin\.""".r
    def parse(s: String): Requirement =
      s match {
        case RegEx(first, second) =>
          Requirement(
            Step(second.toList.singleElementUnsafe),
            Step(first.toList.singleElementUnsafe),
          )
        case _                    => sys.error(s"Failed to parse $s")
      }
  }

  type Parsed = List[Requirement]

  def readFileAndParse(fileName: String): Parsed =
    parseFileLines(fileName, Requirement.parse)

  private def buildDependencyMap(data: Parsed): Map[Step, Set[Step]] =
    data.groupBy(_.step).map { case (k, v) =>
      k -> v.map(_.requires).toSet
    }

  private def tasksReadyToGo(
    dependencies: Map[Step, Set[Step]],
    remaining: Set[Step],
    finished: Set[Step],
  ): List[Step] =
    remaining
      .filter { step =>
        val requirements = dependencies.getOrElse(step, Set.empty)
        val missing      = requirements -- finished
        missing.isEmpty
      }
      .toList
      .sorted

  private def allStepsSet(data: Parsed): Set[Step] =
    data.map(_.step).toSet ++ data.map(_.requires).toSet

  def part1(data: Parsed): String = {
    val dependencies: Map[Step, Set[Step]] = buildDependencyMap(data)

    val allSteps = allStepsSet(data)

    @tailrec
    def makeWaves(
      remaining: Set[Step],
      finished: Set[Step],
      acc: Vector[Step],
    ): Vector[Step] =
      if (remaining.isEmpty) {
        acc
      } else {
        val readyToGo = tasksReadyToGo(dependencies, remaining, finished)
        val selected  = readyToGo.head
        makeWaves(remaining - selected, finished + selected, acc :+ selected)
      }

    val waves = makeWaves(allSteps, Set.empty, Vector.empty)

    waves.map(_.value).mkString
  }

  def part2(data: Parsed, workers: Int, additionalCost: Int): Int = {
    val dependencies: Map[Step, Set[Step]] = buildDependencyMap(data)

    val allSteps = allStepsSet(data)

    @tailrec
    def f(
      nextSecond: Int,
      remaining: Set[Step],
      finished: Set[Step],
      inProgress: List[(Int, Step)],
    ): Int = {
      val currentSecond = nextSecond - 1
      println(
        s"currentSecond = $currentSecond, nextSecond = $nextSecond, remaining = $remaining, finished = $finished, inProgress = $inProgress"
      )
      if (remaining.isEmpty && inProgress.isEmpty) {
        nextSecond
      } else {
        val (finishedNow, remainingInProgress) = inProgress.partition {
          case (finish, _) => nextSecond >= finish
        }
        val finishedSteps                      = finishedNow.map { case (_, step) => step }
        val newFinished                        = finished ++ finishedSteps

        val freeSpots = Math.max(0, workers - remainingInProgress.length)

        val readyToGo = tasksReadyToGo(dependencies, remaining, finished)

        val letsGoTasks                = readyToGo.take(freeSpots)
        val letsGoTasksWithFinishTimes = letsGoTasks map { task =>
          val finishedAt = task.duration(additionalCost) + currentSecond
          (finishedAt, task)
        }

        val newInProgress = remainingInProgress ++ letsGoTasksWithFinishTimes
        val newRemaining  = remaining -- finishedSteps -- letsGoTasks

        f(nextSecond + 1, newRemaining, newFinished, newInProgress)
      }
    }

    f(0, allSteps, Set.empty, Nil)
  }

  def main(args: Array[String]): Unit = {
    val test = readFileAndParse("2018/07-test.txt")
    val real = readFileAndParse("2018/07.txt")

    part1(test) shouldEqual "CABDFE"
    part1(real) shouldEqual "BCADPVTJFZNRWXHEKSQLUYGMIO"

    part2(test, 2, 0) shouldEqual 15
    part2(real, 5, 60) shouldEqual 973
  }
}
