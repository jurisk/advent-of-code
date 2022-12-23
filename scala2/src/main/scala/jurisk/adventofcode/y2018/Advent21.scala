package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent21 {
  def part1(program: Program): Int =
    Simulation.run(Registers.empty(6)) { registers =>
      if (registers(program.ipRegister) == 28) {
        println(s"Part 1 found: ${registers(1)}")
        registers(1).asLeft
      } else {
        program.next(registers).get.asRight
      }
    }

  def part2(program: Program): Int = {
    var previous       = 0
    var seen: Set[Int] = Set.empty

    Simulation.runWithIterationCount(Registers.empty(6)) {
      case (registers, _) =>
        def next: Either[Int, Registers] = program.next(registers) match {
          case Some(newRegisters) => newRegisters.asRight
          case None               => s"Did not expect program to terminate: $registers".fail
        }

        if (registers(program.ipRegister) == 28) {
          val r1 = registers(1)
          if (seen.contains(r1)) {
            println(s"Part 2 found: $previous")
            previous.asLeft
          } else {
            seen = seen + r1
            previous = r1
            if (seen.size % 1000 == 0) {
              println(s"Seen is ${seen.size} elements")
            }
            next
          }
        } else {
          next
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val realData = readFileText("2018/21.txt")

    val real = Program.parse(realData)

    part1(real) shouldEqual 4682012
    part2(real) shouldEqual 5363733
  }
}
