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
      val results = candidates.filter { instruction =>
        val result = instruction.execute(before)
        println(s"Testing $instruction:")
        println(s"Obtained: $result")
        println(s"Expected: $after")
        println
        result == after
      }

      println(s"${results.length} instructions match $this:")
      results foreach println
      println

      results
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
      MulR,
      MulI,
      BAnR,
      BAnI,
      BOrR,
      BOrI,
      SetR,
      SetI,
      GtIR,
      GtRI,
      GtRR,
      EqIR,
      EqRI,
      EqRR,
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

    case class MulR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) * registers(b)
    }

    case class MulI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) * b
    }

    case class BAnR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) & registers(b)
    }

    case class BAnI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) & b
    }

    case class BOrR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) | registers(b)
    }

    case class BOrI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) | b
    }

    case class SetR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a)
    }

    case class SetI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = a
    }

    case class GtIR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (a > registers(b)) 1 else 0
    }

    case class GtRI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (registers(a) > b) 1 else 0
    }

    case class GtRR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (registers(a) > registers(b)) 1 else 0
    }

    case class EqIR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (a == registers(b)) 1 else 0
    }

    case class EqRI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (registers(a) == b) 1 else 0
    }

    case class EqRR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = if (registers(a) == registers(b)) 1 else 0
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
    part1(real) shouldEqual 12345678 // not 101

    part2(test) shouldEqual "asdf"
    part2(real) shouldEqual "asdf"
  }
}
