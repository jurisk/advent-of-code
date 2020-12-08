import Advent08.Instruction
import AdventApp.ErrorMessage
import cats.implicits._

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
    def instructionAt(idx: InstructionIdx): Option[Instruction] =
      instructions.lift(idx)
    
    def filterInstructions[B <: Instruction](
      pf: PartialFunction[Instruction, B],
    ): List[(B, InstructionIdx)] =
      instructions
        .zipWithIndex
        .toList
        .collect { case (x, idx) if pf.isDefinedAt(x) => pf(x) -> idx }
      
    def replaceInstruction(instructionId: InstructionIdx, instruction: Instruction): Program =
      copy(instructions = instructions.updated(instructionId, instruction))
  
  object Program:
    def apply(instructions: Seq[Instruction]): Program = Program(instructions.toVector)
  
  case class Execution(
    program: Program,
    accumulator: Accumulator = 0,
    nextInstructionId: InstructionIdx = 0,
  ):
    def run(alreadyVisited: Set[InstructionIdx] = Set.empty): ExecutionResult =
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

  def fileName: String = "08.txt"

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
    
    val jmps = program.filterInstructions { case x: Instruction.Jmp => x }
    val changeOneJumpToNop: List[Program] = jmps.map { (jmp, jmpIdx) =>
      program.replaceInstruction(jmpIdx, Instruction.Nop(jmp.jump))
    }
    
    val nops = program.filterInstructions { case x: Instruction.Nop => x }
    val changeOneNopToJump: List[Program] = nops.map { (nop, nopIdx) =>
      program.replaceInstruction(nopIdx, Instruction.Jmp(nop.value))
    }

    val potentiallyFixedPrograms = changeOneJumpToNop ++ changeOneNopToJump
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
  
  def parseLine(line: String): Either[ErrorMessage, Instruction] =
    line match
      case AccR(x) =>
        x.toIntOption.map(Instruction.Acc(_)).toRight(ErrorMessage(s"Failed to parse $x"))

      case JmpR(x) =>
        x.toIntOption.map(Instruction.Jmp(_)).toRight(ErrorMessage(s"Failed to parse $x"))

      case NopR(x) =>
        x.toIntOption.map(Instruction.Nop(_)).toRight(ErrorMessage(s"Failed to parse $x"))
        
      case _ => 
        ErrorMessage(line).asLeft
