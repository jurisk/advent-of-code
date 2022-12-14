package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ArrayBuffer

object Advent14 {
  case class State(
    data: ArrayBuffer[Byte], // Mutable!!!
    firstIdx: Int,
    secondIdx: Int,
  ) {
    override def toString: String =
      data.zipWithIndex.map { case (elem, idx) =>
        val (open, close) = (idx == firstIdx, idx == secondIdx) match {
          case (true, true)   => ('{', '}')
          case (true, false)  => ('(', ')')
          case (false, true)  => ('[', ']')
          case (false, false) => (' ', ' ')
        }

        s"$open$elem$close"
      }.mkString

    def next: State = {
      val together: Byte = (data(firstIdx) + data(secondIdx)).toByte

      together match {
        case n if n >= 10 =>
          data += (n / 10).toByte
          data += (n % 10).toByte
        case n            =>
          data += n
      }

      val newFirstIdx  = firstIdx + data(firstIdx) + 1
      val newSecondIdx = secondIdx + data(secondIdx) + 1

      State(
        data = data,
        firstIdx = newFirstIdx   % data.length,
        secondIdx = newSecondIdx % data.length,
      )
    }
  }

  object State {
    def start: State = State(
      data = ArrayBuffer(3, 7),
      firstIdx = 0,
      secondIdx = 1,
    )
  }

  def part1(input: Int): String = {
    val iterations = input + 10

    val resulting = Simulation.runWithIterationCount(State.start) {
      case (state, iteration) =>
        if (iteration < iterations) state.next.asRight else state.asLeft
    }

    resulting.data.slice(input, input + 10).map(_.toString).mkString
  }

  def part2(input: String): Int = {
    val pattern: ArrayBuffer[Byte] = ArrayBuffer.from(
      input.chars().map(x => x - '0').toArray.map(_.toByte).toList
    )

    Simulation.runWithIterationCount(State.start) { case (state, iteration) =>
      if (iteration % 1000000 == 0) {
        println(
          s"At $iteration the size is ${state.data.length} and indices are at ${state.firstIdx} and ${state.secondIdx}"
        )
      }

      // We could have addeded one digit
      val found1: ArrayBuffer[Byte] =
        state.data.slice(state.data.length - pattern.length, state.data.length)

      // We could have added two digits
      val found2: ArrayBuffer[Byte] =
        state.data.slice(
          state.data.length - pattern.length - 1,
          state.data.length - 1,
        )

      if (found1 == pattern) {
        (state.data.length - pattern.length).asLeft
      } else if (found2 == pattern) {
        (state.data.length - pattern.length - 1).asLeft
      } else {
        state.next.asRight
      }
    }
  }

  def main(args: Array[String]): Unit = {
    part1(5) shouldEqual "0124515891"
    part1(9) shouldEqual "5158916779"
    part1(18) shouldEqual "9251071085"
    part1(2018) shouldEqual "5941429882"
    part1(640441) shouldEqual "1041411104"

    part2("01245") shouldEqual 5
    part2("51589") shouldEqual 9
    part2("92510") shouldEqual 18
    part2("59414") shouldEqual 2018
    part2("640441") shouldEqual 20174745
  }
}
