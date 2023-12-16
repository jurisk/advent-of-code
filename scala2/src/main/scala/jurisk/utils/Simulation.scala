package jurisk.utils

import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object Simulation {
  private type Counter =
    Long // We were doing `Counter : Numeric` at one point, but it was a hassle on the caller side

  def runNIterations[State](initial: State, iterations: Counter)(
    f: (State, Counter) => State
  ): State =
    runWithIterationCount(initial) { case (state, iteration) =>
      if (iteration < iterations) {
        f(state, iteration).asRight
      } else {
        state.asLeft
      }
    }

  def runUntilStableStateWithCounter[State](initial: State)(
    f: (State, Counter) => State
  ): (State, Counter) =
    runWithIterationCount(initial) { case (state, iteration) =>
      val newState = f(state, iteration)
      if (newState == state) {
        (state, iteration + 1).asLeft
      } else {
        newState.asRight
      }
    }

  def runUntilStableState[State](initial: State)(f: State => State): State = {
    val (result, _) = runUntilStableStateWithCounter(initial) {
      case (state, _) =>
        f(state)
    }
    result
  }

  def run[State, Result](state: State)(
    f: State => Either[Result, State]
  ): Result =
    runWithIterationCount(state) { case (state, _) =>
      f(state)
    }

  @tailrec
  def runWithIterationCount[State, Result](
    initial: State,
    iterationCount: Counter = 0L,
  )(f: (State, Counter) => Either[Result, State]): Result =
    f(initial, iterationCount) match {
      case Left(result)    => result
      case Right(newState) =>
        runWithIterationCount(newState, iterationCount + 1L)(f)
    }

  def runNIterationsRemovingLoops[State](initial: State, iterations: Long)(
    f: (State, Counter) => State
  ): State = {
    // This can be improved to just finish the remaining part of the last loop and save a lot of iterations, but it
    // makes actual little difference and this will be easier to debug

    val maybeLoop = Simulation.detectLoop(initial, iterations) {
      case (state, iteration) =>
        f(state, iteration)
    }

    val iterationsWithLoopingRemoved = maybeLoop match {
      case Some((loopStart, loopEnd)) =>
        val loopSize = loopEnd - loopStart
        val tailSize = iterations - loopStart
        loopStart + tailSize % loopSize

      case None => iterations
    }

    Simulation.runNIterations(initial, iterationsWithLoopingRemoved) {
      case (state, iteration) =>
        f(state, iteration)
    }
  }

  /** @return The counters at which states repeated */
  private def detectLoop[State](initial: State, iterations: Long)(
    f: (State, Counter) => State
  ): Option[(Counter, Counter)] = {
    val alreadySeen: mutable.HashMap[State, Counter] = mutable.HashMap.empty

    runWithIterationCount(initial) { case (state, iteration) =>
      if (iteration < iterations) {
        alreadySeen.get(state) match {
          case Some(iterationWeSawThisBefore) =>
            (iterationWeSawThisBefore, iteration).some.asLeft
          case None                           =>
            alreadySeen.update(state, iteration)
            f(state, iteration).asRight
        }
      } else {
        none.asLeft
      }
    }
  }

  /** If loop exists before reaching final state, returns
    * Right(previous_iteration, this_iteration) where states are identical in
    * both of these iterations.
    *
    * If final state is reached before finding a loop, returns Left(result),
    * where result is the result returned by `f`.
    */
  def detectLoop[State, Result](initial: State)(
    f: (State, Counter) => Either[Result, State]
  ): Either[Result, (Counter, Counter)] = {
    val alreadySeen: mutable.HashMap[State, Counter] = mutable.HashMap.empty
    runWithIterationCount(initial) { case (state, iteration) =>
      alreadySeen.get(state) match {
        case Some(iterationWeSawThisBefore) =>
          (iterationWeSawThisBefore, iteration).asRight.asLeft
        case None                           =>
          alreadySeen.update(state, iteration)
          f(state, iteration).leftMap(_.asLeft)
      }
    }
  }
}
