package jurisk.adventofcode.y2018

import jurisk.adventofcode.y2018.Advent18.Square._
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent18 {
  type Parsed = Field2D[Square]

  sealed trait Square {
    def toChar: Char
  }
  object Square       {
    final case object Open       extends Square {
      val toChar: Char = '.'
    }
    final case object Lumberyard extends Square {
      val toChar: Char = '#'
    }
    final case object Trees      extends Square {
      val toChar: Char = '|'
    }

    def parse(ch: Char): Square =
      ch match {
        case Open.toChar       => Open
        case Trees.toChar      => Trees
        case Lumberyard.toChar => Lumberyard
        case _                 => "Did not recognize '$ch'".fail
      }
  }

  def parse(data: String): Parsed =
    Field2D.parseFromString(data, Square.parse)

  private def next(state: Field2D[Square]): Field2D[Square] =
    state.mapByCoordsWithValues { case (c, v) =>
      val adjacent8: Map[Square, Int] =
        state.adjacent8Values(c).counts.withDefaultValue(0)

      v match {
        case Open       => if (adjacent8(Trees) >= 3) Trees else Open
        case Trees      => if (adjacent8(Lumberyard) >= 3) Lumberyard else Trees
        case Lumberyard =>
          if (adjacent8(Lumberyard) >= 1 && adjacent8(Trees) >= 1) Lumberyard
          else Open
      }
    }

  def part1(initial: Parsed, times: Long): Int = {
    val result = Simulation.runNIterations(initial, times) { case (state, _) =>
      next(state)
    }

    result.count(_ == Trees) * result.count(_ == Lumberyard)
  }

  def part2(initial: Parsed, times: Long): Int = {
    val (loopAt1, loopAt2) = Simulation.detectLoop(initial) { case (state, _) =>
      next(state)
    }

    val loopSize = loopAt2 - loopAt1
    val tail     = times - loopAt1
    part1(initial, loopAt1 + tail % loopSize)
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/18-test.txt")
    val realData = readFileText("2018/18.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test, 10) shouldEqual 1147
    part1(real, 10) shouldEqual 763804

    part2(real, 1000000000) shouldEqual 188400
  }
}
