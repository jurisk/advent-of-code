package jurisk.algorithms

import cats.implicits._

// See https://en.wikipedia.org/wiki/Backtracking
trait Backtracking[P, C] {
  def root(p: P): C
  def reject(p: P, c: C): Boolean
  def accept(p: P, c: C): Boolean
  def extensions(p: P, c: C): Seq[C]
}

object Backtracker {
  def solve[P, C](backtracking: Backtracking[P, C])(p: P): Option[C] = {
    import backtracking._

    def backtrack(c: C): Option[C] =
      if (reject(p, c)) {
        none
      } else if (accept(p, c)) {
        c.some
      } else {
        extensions(p, c) foreach { next =>
          val result = backtrack(next)

          if (result.isDefined) {
            return result
          }
        }

        none
      }

    backtrack(root(p))
  }
}
