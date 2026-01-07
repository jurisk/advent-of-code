package jurisk.utils

object Timing {
  def timed[T](label: String)(block: => T): T = {
    val startTime = System.currentTimeMillis()
    val result    = block
    val elapsed   = System.currentTimeMillis() - startTime
    println(s"$label (${elapsed}ms)")
    result
  }

  def timedWithResult[T](label: String, resultLabel: T => String)(
    block: => T
  ): T = {
    val startTime = System.currentTimeMillis()
    val result    = block
    val elapsed   = System.currentTimeMillis() - startTime
    println(s"$label: ${resultLabel(result)} (${elapsed}ms)")
    result
  }
}
