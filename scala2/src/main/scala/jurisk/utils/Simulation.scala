package jurisk.utils

import scala.annotation.tailrec

object Simulation {
  def run[State, Result](state: State)(
    f: State => Either[Result, State]
  ): Result =
    runWithIterationCount(state) { case (state, _) =>
      f(state)
    }

  @tailrec
  def runWithIterationCount[State, Result](
    state: State,
    iterationCount: Int = 0,
  )(f: (State, Int) => Either[Result, State]): Result =
    f(state, iterationCount) match {
      case Left(result)    => result
      case Right(newState) =>
        runWithIterationCount(newState, iterationCount + 1)(f)
    }
}
