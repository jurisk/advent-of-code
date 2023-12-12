package jurisk.math

object Combinatorics {
  def fillWildcards[T](
    input: List[T],
    wildcard: T,
    replacements: List[T],
  ): List[List[T]] = {
    def f(rem: List[T]): List[List[T]] =
      rem match {
        case head :: tail if head == wildcard =>
          f(tail) flatMap { result =>
            replacements map { replacement =>
              replacement :: result
            }
          }

        case head :: tail =>
          f(tail) map { result =>
            head :: result
          }

        case Nil => List(List())
      }

    f(input)
  }
}
