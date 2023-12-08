package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent12 {
  final case class State(
    pots: List[Boolean],
    zeroAt: Int,
  ) {
    override def toString: String = {
      val potString = pots.map {
        case true  => '#'
        case false => '.'
      }.mkString

      s"$potString, zeroAt = $zeroAt"
    }

    @tailrec
    private def dropEmptyTail: State =
      pots.lastOption match {
        case Some(false) => State(pots.init, zeroAt).dropEmptyTail
        case _           => this
      }

    @tailrec
    private def normalForm: State =
      pots match {
        case Nil           => State(pots, 0)
        case true :: _     => this
        case false :: tail => State(tail, zeroAt - 1).normalForm
      }

    def apply(patterns: Map[List[Boolean], Boolean]): State = {
      val newPots = (-4 until pots.length + 4).toList map { idx =>
        val segment = (idx until idx + 5).toList map { i =>
          pots.lift(i).getOrElse(false)
        }

        val result = patterns.getOrElse(segment, false)

        result
      }

      State(newPots, zeroAt = zeroAt + 2).dropEmptyTail.normalForm
    }

    def numberSum: Long =
      pots.zipWithIndex.map { case (pot, idx) =>
        if (pot) idx - zeroAt else 0
      }.sum
  }

  private def parsePots(s: String): List[Boolean] =
    s.toList.map {
      case '#' => true
      case '.' => false
      case ch  => sys.error(s"Unrecognized $ch")
    }

  object State {
    def parse(s: String): State = {
      val pots = parsePots(s.removePrefixUnsafe("initial state: "))

      State(pots, 0)
    }
  }

  private def parsePattern(s: String): (List[Boolean], Boolean) = {
    val (a, b) = s.splitPairUnsafe(" => ")
    parsePots(a) -> parsePots(b).singleElementUnsafe
  }

  type Patterns = Map[List[Boolean], Boolean]
  type Parsed   = (State, Patterns)

  def parse(data: String): Parsed = {
    val (a, b) = data.splitPairUnsafe("\n\n");
    (State.parse(a), b.parseLines(parsePattern).toMap)
  }

  def solve(data: Parsed, iterations: Long): Long = {
    val (initialState, patterns) = data

    Simulation.runWithIterationCount(initialState) { case (state, iteration) =>
      if (iteration >= iterations) {
        state.numberSum.asLeft
      } else {
        val newState = state.apply(patterns)

        // In theory, a longer loop could exist (or no loop), but the test data has a simple immediate loop
        if (state.pots == newState.pots) { // we have a loop in pots (though not in zeroAt)
          val prevScore           = state.numberSum
          val thisScore           = newState.numberSum
          val diff                = thisScore - prevScore
          val iterationsRemaining = iterations - iteration
          val result              = prevScore + iterationsRemaining * diff
          result.asLeft
        } else {
          newState.asRight
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/12-test.txt")
    val realData = readFileText("2018/12.txt")

    val test = parse(testData)
    val real = parse(realData)

    solve(test, 20) shouldEqual 325
    solve(real, 20) shouldEqual 3241

    solve(real, 50000000000L) shouldEqual 2749999999911L
  }
}
