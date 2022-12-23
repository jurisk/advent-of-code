package jurisk.adventofcode.y2018

import cats.implicits.catsSyntaxEitherId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent19 {
  def part1(program: Program): Int = {
    program.debugPrint()

    val result = Simulation.runWithIterationCount(Registers.empty(6)) {
      case (registers, iteration) =>
        program.next(registers) match {
          case Some(newRegisters) =>
            if (iteration % 10_000_000 == 0) {
              println(
                s"ip = ${registers(program.ipRegister)} $registers ${program
                    .instructions(program.ipRegister)} $newRegisters"
              )
            }
            newRegisters.asRight
          case None               => registers.asLeft
        }
    }

    result(0)
  }

  def part2(program: Program): Int = {
    program.debugPrint()

    val numberToFactor =
      Simulation.runWithIterationCount(Registers.empty(6).setRegister(0, 1)) {
        case (registers, _) =>
          if (registers(program.ipRegister) == 3) { // the loop starts
            registers(3).asLeft
          } else {
            program.next(registers) match {
              case Some(newRegisters) =>
                newRegisters.asRight
              case None               =>
                "Unexpected finish".fail
            }
          }
      }

    val factors = (1 to numberToFactor).filter(x => numberToFactor % x == 0)
    factors.sum
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/19-test.txt")
    val realData = readFileText("2018/19.txt")

    val test = Program.parse(testData)
    val real = Program.parse(realData)

    part1(test) shouldEqual 7
    part1(real) shouldEqual 2280

    part2(real) shouldEqual 30481920
  }
}
