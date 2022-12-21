package jurisk.utils

import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object Simulation {
  type Counter = Long // We were doing `Counter : Numeric` at one point, but it was a hassle on the caller side

  def runNIterations[State](state: State, iterations: Counter)(f: (State, Counter) => State): State = {
    runWithIterationCount(state) { case (state, iteration) =>
      if (iteration < iterations) {
        f(state, iteration).asRight
      } else {
        state.asLeft
      }
    }
  }

  def runUntilStableState[State](state: State)(f: (State, Counter) => State): State = {
    runWithIterationCount(state) { case (state, iteration) =>
      val newState = f(state, iteration)
      if (newState == state) {
        state.asLeft
      } else {
        newState.asRight
      }
    }
  }

  def run[State, Result](state: State)(
    f: State => Either[Result, State]
  ): Result =
    runWithIterationCount(state) { case (state, _) =>
      f(state)
    }

  @tailrec
  def runWithIterationCount[State, Result](
    state: State,
    iterationCount: Counter = 0L,
  )(f: (State, Counter) => Either[Result, State]): Result = {
    f(state, iterationCount) match {
      case Left(result)    => result
      case Right(newState) =>
        runWithIterationCount(newState, iterationCount + 1L)(f)
    }
  }

  /** @return The counters at which states repeated */
  def detectLoop[State](initial: State)(f: (State, Counter) => State): (Counter, Counter) = {
    val alreadySeen: mutable.HashMap[State, Counter] = mutable.HashMap.empty

    runWithIterationCount(initial) { case (state, iteration) =>
      alreadySeen.get(state) match {
        case Some(iterationWeSawThisBefore) => (iterationWeSawThisBefore, iteration).asLeft
        case None =>
          alreadySeen.update(state, iteration)
          f(state, iteration).asRight
      }
    }
  }
}
