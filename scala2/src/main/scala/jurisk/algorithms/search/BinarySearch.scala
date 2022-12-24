package jurisk.algorithms.search

import scala.annotation.tailrec
import scala.math.Integral.Implicits._
import scala.math.Ordering.Implicits._

object BinarySearch {
  @tailrec
  def binarySearchForLowestValidN[Result, N: Integral](
    lower: N,
    upper: N,
    f: N => Option[Result],
  ): (N, Result) = {
    val One = implicitly[Numeric[N]].one
    val Two = One + One

    require(lower < upper)
    require(f(lower).isEmpty)

    @tailrec
    def search(lower: N, upper: N, upperResult: Result): (N, Result) =
      if (lower + One == upper) {
        (upper, upperResult)
      } else {
        val mid = (lower + upper) / Two
        f(mid) match {
          case Some(midResult) => search(lower, mid, midResult)
          case None            => search(mid, upper, upperResult)
        }
      }

    f(upper) match {
      case Some(upperResult) => search(lower, upper, upperResult)
      case None              => binarySearchForLowestValidN(lower, upper * Two, f)
    }
  }
}
