package jurisk.algorithms

import cats.implicits._

import scala.annotation.tailrec

// See https://en.wikipedia.org/wiki/Backtracking
trait Backtracking[P, C] {
  def root(p: P): C
  def reject(p: P, c: C): Boolean
  def accept(p: P, c: C): Boolean
  def extensions(p: P, c: C): Seq[C]
  def visit(p: P, c: C): Unit = ()
}

object Backtracker {
  def solve[P, C](backtracking: Backtracking[P, C])(p: P): Option[C] = {
    import backtracking._

    @tailrec
    def backtrack(stack: Vector[C]): Option[C] = stack match {
      case c +: cs =>
        visit(p, c)
        if (reject(p, c)) {
          backtrack(cs)
        } else if (accept(p, c)) {
          c.some
        } else {
          backtrack(extensions(p, c).toVector ++ cs)
        }
      case _       => none
    }

    backtrack(Vector(root(p)))
  }
}
