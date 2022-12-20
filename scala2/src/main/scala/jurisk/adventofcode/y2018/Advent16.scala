package jurisk.adventofcode.y2018

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Advent16 {
  type Parsed = (List[Sample], List[InstructionBinary])

  final case class Registers(data: ArraySeq[Int]) {
    require(data.length == 4)

    def apply(index: Int): Int =
      data(index)

    def setRegister(index: Int, value: Int): Registers =
      Registers(
        data.updated(index, value)
      )
  }

  object Registers {
    def apply(a: Int, b: Int, c: Int, d: Int): Registers =
      Registers(
        ArraySeq(a, b, c, d)
      )

    val Start: Registers = Registers(0, 0, 0, 0)
  }

  final case class InstructionBinary(data: ArraySeq[Int]) {
    require(data.length == 4)

    def toInstruction(opcodeMapping: Map[Int, InstructionName]): Instruction = {
      val instructionName = opcodeMapping(data.head)
      Instruction.instructionFactoryByName(instructionName)(
        data(1),
        data(2),
        data(3),
      )
    }

    def opcode: Int            = data.head
    def apply(index: Int): Int = data(index)
  }

  object InstructionBinary {
    def parse(s: String): InstructionBinary = InstructionBinary(
      ArraySeq.from(s.split(" ").map(_.toInt))
    )
  }

  final case class Sample(
    before: Registers,
    instructionBinary: InstructionBinary,
    after: Registers,
  ) {
    def instructionsThisMayMatch: Iterable[Instruction] = {
      val candidates =
        Instruction.allOptionsFromInstructionBinary(instructionBinary)
      val results    = candidates.filter { instruction =>
        val result = instruction.execute(before)
        println(s"Testing $instruction:")
        println(s"Obtained: $result")
        println(s"Expected: $after")
        println
        result == after
      }

      println(s"${results.size} instructions match $this:")
      results foreach println
      println

      results
    }
  }

  final case class InstructionName(name: String) extends AnyVal

  sealed trait Instruction {
    val a: Int
    val b: Int
    val c: Int

    def instructionName: InstructionName = InstructionName(
      getClass.getSimpleName
    )

    def execute(registers: Registers): Registers =
      registers.setRegister(c, whatToStoreInC(registers))

    def whatToStoreInC(registers: Registers): Int
  }

  type InstructionFactoryF = (Int, Int, Int) => Instruction

  case object Instruction {
    private val AllInstructions: Map[InstructionName, InstructionFactoryF] =
      List(
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
      ).map { f =>
        f(0, 0, 0).instructionName -> f
      }.toMap

    def instructionFactoryByName(
      instructionName: InstructionName
    ): InstructionFactoryF = AllInstructions(instructionName)

    def allOptionsFromInstructionBinary(
      binary: InstructionBinary
    ): Iterable[Instruction] = AllInstructions.values.map { instruction =>
      instruction(binary(1), binary(2), binary(3))
    }

    case class AddR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        registers(a) + registers(b)
    }

    case class AddI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) + b
    }

    case class MulR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        registers(a) * registers(b)
    }

    case class MulI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) * b
    }

    case class BAnR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        registers(a) & registers(b)
    }

    case class BAnI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int = registers(a) & b
    }

    case class BOrR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        registers(a) | registers(b)
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
      override def whatToStoreInC(registers: Registers): Int =
        if (a > registers(b)) 1 else 0
    }

    case class GtRI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        if (registers(a) > b) 1 else 0
    }

    case class GtRR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        if (registers(a) > registers(b)) 1 else 0
    }

    case class EqIR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        if (a == registers(b)) 1 else 0
    }

    case class EqRI(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        if (registers(a) == b) 1 else 0
    }

    case class EqRR(a: Int, b: Int, c: Int) extends Instruction {
      override def whatToStoreInC(registers: Registers): Int =
        if (registers(a) == registers(b)) 1 else 0
    }
  }

  object Sample {
    def parse(s: String): Sample = {
      val Array(beforeString, instructionString, afterString) = s.split("\n")
      val beforeNumbers                                       = beforeString match {
        case s"Before: [$data]" => data.split(", ").map(_.toInt)
      }

      val instructionNumbers = instructionString.split(" ").map(_.toInt)

      val afterNumbers = afterString match {
        case s"After:  [$data]" => data.split(", ").map(_.toInt)
      }

      Sample(
        Registers(ArraySeq.from(beforeNumbers)),
        InstructionBinary(ArraySeq.from(instructionNumbers)),
        Registers(ArraySeq.from(afterNumbers)),
      )
    }
  }

  def parse(data: String): Parsed = {
    val Array(samplesString, programString) = data.split("\n\n\n\n")
    val samples                             = samplesString.parseList("\n\n", Sample.parse)
    val program                             = programString.split("\n").map(InstructionBinary.parse).toList
    (samples, program)
  }

  def part1(data: Parsed): Int = {
    val (samples, _) = data
    println(samples)
    samples.count(_.instructionsThisMayMatch.size >= 3)
  }

  @tailrec
  private def solveOpcodes(
    options: Map[Int, Set[InstructionName]]
  ): Map[Int, InstructionName] = {
    // We know the mapping Int -> InstructionName for these names
    val solvedNames = options
      .filter { case (_, v) => v.size == 1 }
      .map { case (_, v) => v.singleElementUnsafe }
      .toSet
    val unsolved    = options.size - solvedNames.size
    if (unsolved == 0) { // everything is solved
      options.map { case (k, v) => k -> v.singleElementUnsafe }
    } else {
      // reduce the number of options where possible
      val reduced = options.map { case (k, v) =>
        if (v.size > 1) { k -> (v -- solvedNames) }
        else { k -> v }
      }
      solveOpcodes(reduced)
    }
  }

  private def solveOpcodes(
    input: List[(Int, Set[InstructionName])]
  ): Map[Int, InstructionName] = {
    val grouped: Map[Int, List[Set[InstructionName]]] =
      input.groupBy { case (k, _) => k }.map { case (k, v) => k -> v.map(_._2) }
    val options: Map[Int, Set[InstructionName]]       = grouped.map {
      case (opcode, potentialInstructions) =>
        opcode -> potentialInstructions.reduce(_ intersect _)
    }
    solveOpcodes(options)
  }

  def part2(data: Parsed): Int = {
    val (samples, programBinary)                 = data
    val opcodeToPotentialNames                   = samples.map { sample =>
      sample.instructionBinary.opcode -> sample.instructionsThisMayMatch
        .map(_.instructionName)
        .toSet
    }
    val opcodeMapping: Map[Int, InstructionName] = solveOpcodes(
      opcodeToPotentialNames
    )
    val instructions                             = programBinary.map(_.toInstruction(opcodeMapping))
    val result                                   = instructions.foldLeft(Registers.Start) {
      case (acc, instruction) =>
        instruction.execute(acc)
    }
    result(0)
  }

  def main(args: Array[String]): Unit = {
    val testData =
      "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n\n\n\n0 0 0 0".stripMargin
    val realData = readFileText("2018/16.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 1
    part1(real) shouldEqual 642

    part2(real) shouldEqual 481
  }
}
