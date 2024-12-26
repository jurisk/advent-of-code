package jurisk.adventofcode.y2024

import Advent24._
import cats.implicits.{catsSyntaxFoldableOps0, catsSyntaxOptionId}
import jurisk.collections.immutable.SetOfTwo
import jurisk.utils.Parsing.StringOps
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec
import scala.util.Random

class Advent24Spec extends AnyFreeSpec {
  private def testData = parseFile(fileName("-test-00"))
  private def realData = parseFile(fileName(""))

  "part 1" - {
    "test" in {
      part1(testData) shouldEqual 2024
    }

    "real" in {
      part1(realData) shouldEqual 58740594706150L
    }
  }

  "part 2" - {
    "real" ignore {
      part2(realData) shouldEqual "cvh,dbb,hbk,kvn,tfn,z14,z18,z23"
    }

    // Not fully successful... It is often stuck in local minima.
    "can fix arbitrary adders" ignore {
      val fixed = {
        val (_, input) = realData
        input.applySwaps(
          Set(
            SetOfTwo("hbk", "z14"),
            SetOfTwo("kvn", "z18"),
            SetOfTwo("dbb", "z23"),
            SetOfTwo("cvh", "tfn"),
          ).map(s => SetOfTwo(s.map(Wire.parse)))
        )
      }
      fixed.errorsOnAddition shouldEqual 0.some

      val outputs = fixed.allOutputs.toIndexedSeq

      @tailrec
      def findValidSwaps(n: Int): Set[SetOfTwo[Wire]] = {
        val candidateSwaps = Random
          .shuffle(outputs)
          .toList
          .grouped(2)
          .map {
            case List(a, b) => SetOfTwo(a, b)
            case _          => "Unexpected".fail
          }
          .take(n)
          .toSet
        val adjusted       = fixed.applySwaps(candidateSwaps)
        if (adjusted.isValid) {
          candidateSwaps
        } else {
          findValidSwaps(n)
        }
      }

      val selectedSwaps = findValidSwaps(4)

      println(s"Selected random swaps: $selectedSwaps")
      val wrongAgain  = fixed.applySwaps(selectedSwaps)
      val resultSwaps = wrongAgain.bestSwaps
      resultSwaps shouldEqual selectedSwaps
      val result      = wrongAgain.applySwaps(resultSwaps)
      result.errorsOnAddition shouldEqual 0.some
    }
  }
}
