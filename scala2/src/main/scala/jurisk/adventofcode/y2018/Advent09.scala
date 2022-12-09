package jurisk.adventofcode.y2018

import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

object Advent09 {
  private def solve(players: Int, lastMarble: Int): Long = {
    var turn: Int           = 0
    val scores: Array[Long] = Array.fill(players)(0)

    val state: mutable.ArrayDeque[Int] =
      new mutable.ArrayDeque(initialSize = lastMarble)
    state.append(0)

    var lastTs = System.currentTimeMillis()

    while (turn <= lastMarble) {
      turn = turn + 1

      if (turn % 100_000 == 0) {
        val now = System.currentTimeMillis()
        println(
          s"Working... turn = $turn, took ${now - lastTs} ms, ${turn.toDouble / lastMarble} complete"
        )
        lastTs = now
      }

      if (turn % 23 == 0) {
        val tmp           = (0 until 6).map(_ => state.removeLast()).reverse
        val removedMarble = state.removeLast()
        state.prependAll(tmp)

        val playerId = turn % players
        scores(playerId) = scores(playerId) + turn + removedMarble
      } else {
        if (state.size == 1) {
          state.prepend(turn)
        } else {
          val a = state.remove(0)
          val b = state.remove(0)
          state.prepend(turn)
          state.addOne(a)
          state.addOne(b)
        }
      }
    }

    scores.max
  }

  def main(args: Array[String]): Unit = {
    solve(9, 25) shouldEqual 32
    solve(10, 1618) shouldEqual 8317
    solve(13, 7999) shouldEqual 146373
    solve(17, 1104) shouldEqual 2764
    solve(21, 6111) shouldEqual 54718
    solve(30, 5807) shouldEqual 37305

    solve(429, 70901) shouldEqual 399645

    solve(429, 70901 * 100) shouldEqual 3352507536L
  }
}
