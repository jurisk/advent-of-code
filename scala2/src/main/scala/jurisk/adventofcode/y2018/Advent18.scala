package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.adventofcode.y2018.Advent18.Square._
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent18 {
  type Field = Field2D[Square]

  sealed trait Square {
    def toChar: Char
  }
  object Square       {
    case object Open       extends Square {
      val toChar: Char = '.'
    }
    case object Lumberyard extends Square {
      val toChar: Char = '#'
    }
    case object Trees      extends Square {
      val toChar: Char = '|'
    }

    def parse(ch: Char): Square =
      ch match {
        case Open.toChar       => Open
        case Trees.toChar      => Trees
        case Lumberyard.toChar => Lumberyard
        case _                 => s"Did not recognize '$ch'".fail
      }
  }

  def parse(data: String): Field =
    Field2D.parse(data, Square.parse)

  private def next(state: Field): Field =
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

  def solve(initial: Field, times: Long): Long = {
    val result = Simulation.runNIterationsRemovingLoops(initial, times) {
      case (state, _) =>
        next(state)
    }

    result.count(_ == Trees) * result.count(_ == Lumberyard)
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/18-test.txt")
    val realData = readFileText("2018/18.txt")

    val test = parse(testData)
    val real = parse(realData)

    solve(test, 10) shouldEqual 1147
    solve(real, 10) shouldEqual 763804

    solve(real, 1000000000) shouldEqual 188400
  }
}
