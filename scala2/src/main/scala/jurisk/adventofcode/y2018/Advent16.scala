package jurisk.adventofcode.y2018

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Advent16 {
  type Parsed = (List[Sample], List[InstructionBinary])

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
    def allOptionsFromInstructionBinary(
      binary: InstructionBinary
    ): Iterable[Instruction] = Instruction.AllInstructions.values.map {
      instruction =>
        instruction(binary(1), binary(2), binary(3))
    }

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
        InstructionBinary.allOptionsFromInstructionBinary(instructionBinary)
      val results    = candidates.filter { instruction =>
        val result = instruction.execute(before)
        println(s"Testing $instruction:")
        println(s"Obtained: $result")
        println(s"Expected: $after")
        println()
        result == after
      }

      println(s"${results.size} instructions match $this:")
      results foreach println
      println()

      results
    }
  }

  object Sample {
    def parse(s: String): Sample = {
      val Array(beforeString, instructionString, afterString) = s.split("\n")
      val beforeNumbers                                       = beforeString match {
        case s"Before: [$data]" => data.split(", ").map(_.toInt)
        case _                  => beforeString.failedToParse
      }

      val instructionNumbers = instructionString.split(" ").map(_.toInt)

      val afterNumbers = afterString match {
        case s"After:  [$data]" => data.split(", ").map(_.toInt)
        case _                  => afterString.failedToParse
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
    val result                                   = instructions.foldLeft(Registers.empty(4)) {
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
