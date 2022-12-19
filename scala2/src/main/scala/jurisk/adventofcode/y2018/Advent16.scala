package jurisk.adventofcode.y2018

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

object Advent16 {
  type Program = String
  type Parsed = (List[Sample], Program)

  final case class Registers(data: ArraySeq[Int]) {
    require(data.length == 4)

    def apply(index: Int): Int = {
      data(index)
    }

    def setRegister(index: Int, value: Int): Registers = {
      Registers(
        data.updated(index, value)
      )
    }
  }

  object Registers {
    def apply(a: Int, b: Int, c: Int, d: Int): Registers = {
      Registers(
        ArraySeq(a, b, c, d)
      )
    }
  }

  final case class InstructionBinary(data: ArraySeq[Int]) {
    require(data.length == 4)

    def apply(index: Int): Int = data(index)
  }

  final case class Sample(
    before: Registers,
    instructionBinary: InstructionBinary,
    after: Registers,
  ) {
    def instructionsThisMayMatch: List[Instruction] = {
      val candidates = Instruction.allOptionsFromInstructionBinary(instructionBinary)
      candidates.filter { instruction =>
        instruction.execute(before) == after
      }
    }
  }

  sealed trait Instruction {
    val a: Int
    val b: Int
    val c: Int

    def execute(registers: Registers): Registers =
      registers.setRegister(c, whatToStoreInC(registers))

    def whatToStoreInC(registers: Registers): Int
  }

  case object Instruction {
    private val AllInstructions: List[(Int, Int, Int) => Instruction] = List(
      AddR,
      AddI,
    )

    def allOptionsFromInstructionBinary(binary: InstructionBinary): List[Instruction] = AllInstructions.map { instruction =>
      instruction(binary(1), binary(2), binary(3))
    }

    case class AddR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) + registers(b)
    }

    case class AddI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) + b
    }
  }

  object Sample {
    def parse(s: String): Sample = {
      val Array(beforeString, instructionString, afterString) = s.split("\n")
      val beforeNumbers = beforeString match {
        case s"Before: [$data]" => data.split(", ").map(_.toInt)
      }

      val instructionNumbers = instructionString.split(" ").map(_.toInt)

      val afterNumbers = afterString match {
        case s"After:  [$data]" =>  data.split(", ").map(_.toInt)
      }

      Sample(
        Registers(ArraySeq.from(beforeNumbers)),
        InstructionBinary(ArraySeq.from(instructionNumbers)),
        Registers(ArraySeq.from(afterNumbers)),
      )
    }
  }

  def parse(data: String): Parsed = {
    val Array(samplesString, program) = data.split("\n\n\n\n")
    val samples = samplesString.parseList("\n\n", Sample.parse)
    (samples, program)
  }

  def part1(data: Parsed): Int = {
    val (samples, program) = data
    println(samples)
    samples.count(_.instructionsThisMayMatch.length == 3)
  }

  def part2(data: Parsed): String = {
    val (samples, program) = data
    "test"
  }

  def main(args: Array[String]): Unit = {
    val testData = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n\n\n\n ".stripMargin
    val realData = readFileText("2018/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 1
    part1(real) shouldEqual 12345678

    part2(test) shouldEqual "asdf"
    part2(real) shouldEqual "asdf"
  }
}
