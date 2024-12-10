package jurisk.adventofcode.y2020

import cats.implicits.*
import AdventApp.ErrorMessage
import jurisk.adventofcode.y2020.Advent08.{ExecutionResult, Instruction}

import scala.annotation.tailrec

object Advent08 extends SingleLineAdventApp[Instruction, Int]:
  type Accumulator = Int
  type InstructionIdx = Int

  enum Instruction:
    case Acc(delta: Accumulator)
    case Jmp(jump: InstructionIdx)
    case Nop(value: Int)

  enum ExecutionResult:
    case LoopDetected(index: InstructionIdx, accumulator: Accumulator)
    case FinishedSuccessfully(accumulator: Accumulator)

  case class Program(
    private val instructions: Vector[Instruction],
  ):
    def indices: Range = instructions.indices

    def instructionAt(idx: InstructionIdx): Option[Instruction] = instructions.lift(idx)

    def replaceInstruction(instructionIdx: InstructionIdx, instruction: Instruction): Option[Program] = {
      if indices.contains(instructionIdx) then
        copy(instructions = instructions.updated(instructionIdx, instruction)).some
      else
        none
    }

  object Program:
    def apply(instructions: Seq[Instruction]): Program = Program(instructions.toVector)

  case class Execution(
    program: Program,
    accumulator: Accumulator = 0,
    nextInstructionId: InstructionIdx = 0,
  ):
    @tailrec
    final def run(alreadyVisited: Set[InstructionIdx] = Set.empty): ExecutionResult =
      if alreadyVisited.contains(nextInstructionId) then
        ExecutionResult.LoopDetected(nextInstructionId, accumulator)
      else
        executeNext match
          case Some(x) =>
            x.run(alreadyVisited + nextInstructionId)
          case None =>
            ExecutionResult.FinishedSuccessfully(accumulator)

    def executeNext: Option[Execution] =
      program.instructionAt(nextInstructionId).map {
        case Instruction.Acc(delta) => copy(
          accumulator = accumulator + delta,
          nextInstructionId = nextInstructionId + 1,
        )

        case Instruction.Jmp(jump) => copy(
          nextInstructionId = nextInstructionId + jump,
        )

        case Instruction.Nop(_) => copy(
          nextInstructionId = nextInstructionId + 1,
        )
      }

  def exercise: Int = 8

  def toProgram(testCases: List[Instruction]): Program =
    Program(testCases)

  def solution1(testCases: List[Instruction]): Int =
    val program = toProgram(testCases)

    Execution(program).run() match {
      case ExecutionResult.LoopDetected(_, accumulator) => accumulator
      case ExecutionResult.FinishedSuccessfully(accumulator) => sys.error(s"Expected to halt in $program")
    }

  def solution2(testCases: List[Instruction]): Int =
    val program = toProgram(testCases)

    val potentiallyFixedPrograms = program.indices.toList flatMap { idx =>
      program.instructionAt(idx) match
        case Some(Instruction.Jmp(jump)) => // replace a `jmp` with `nop`
          program.replaceInstruction(idx, Instruction.Nop(jump))
        case Some(Instruction.Nop(value)) =>  // replace a `nop` with `jmp`
          program.replaceInstruction(idx, Instruction.Jmp(value))
        case _ =>
          none
    }

    val successes = potentiallyFixedPrograms
      .map { x => Execution(x).run() }
      .collect { case x: ExecutionResult.FinishedSuccessfully => x }

    successes match
      case Nil => sys.error("Substitution failed")
      case x :: Nil => x.accumulator
      case x => sys.error(s"Too many valid results $x")

  private val AccR = """acc ([+|-]\d+)""".r
  private val JmpR = """jmp ([+|-]\d+)""".r
  private val NopR = """nop ([+|-]\d+)""".r

  extension [T](self: String)
    def parseInt(f: Int => T): Either[ErrorMessage, T] =
      self.toIntOption.map(f).toRight(ErrorMessage(s"Failed to parse $self"))

  def parseLine(line: String): Either[ErrorMessage, Instruction] =
    line match
      case AccR(x) => x.parseInt(Instruction.Acc(_))
      case JmpR(x) => x.parseInt(Instruction.Jmp(_))
      case NopR(x) => x.parseInt(Instruction.Nop(_))
      case _ => ErrorMessage(line).asLeft
