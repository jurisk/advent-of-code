package jurisk.adventofcode.y2020

import scala.io.Source

object Advent03 extends App:
  val data = Source.fromResource("03.txt").getLines().toArray
  private val Tree = '#'

  def f(jumpRight: Int, jumpDown: Int): Long =
    (data.indices by jumpDown)
      .zipWithIndex
      .tail
      .count { case (r, idx) =>
        val row = data(r)
        row((jumpRight * idx) % row.length) == Tree
      }

  println(f(3, 1))
  println(f(1, 1) * f(3, 1) * f(5, 1) * f(7, 1) * f(1, 2))
