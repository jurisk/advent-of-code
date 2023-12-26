package jurisk.math

// https://en.wikipedia.org/wiki/Gaussian_elimination
object GaussianElimination {
  def solve(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val n         = A.length
    val augmented = A.zip(b).map { case (row, bi) => row :+ bi }

    for (col <- 0 until n) {
      // Find pivot row
      val pivotRow = (col until n).maxBy(row => math.abs(augmented(row)(col)))
      val temp     = augmented(col)
      augmented(col) = augmented(pivotRow)
      augmented(pivotRow) = temp

      // Make leading coefficient of pivot row 1
      val pivotElement = augmented(col)(col)
      for (j <- col until n + 1)
        augmented(col)(j) /= pivotElement

      // Eliminate below pivot
      for (i <- col + 1 until n) {
        val factor = augmented(i)(col)
        for (j <- col until n + 1)
          augmented(i)(j) -= factor * augmented(col)(j)
      }
    }

    // Back substitution
    val x = new Array[Double](n)
    for (i <- n - 1 to 0 by -1) {
      x(i) = augmented(i)(n)
      for (j <- i + 1 until n)
        x(i) -= augmented(i)(j) * x(j)
    }

    x
  }
}
