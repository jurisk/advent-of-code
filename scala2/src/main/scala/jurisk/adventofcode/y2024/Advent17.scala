package jurisk.adventofcode.y2024

import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId}
import jurisk.math.{pow, pow2}
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent17 {
  type Input  = State
  type Output = String
  type N      = Long

  /*
   * a => (new_a, output)
   *
   * This is entirely bound to our specific program, obtained through reverse engineering it.
   */
  def f(a: Long): (Long, Int) = {
    val output = ((a % 8) ^ 3 ^ (a / pow2((a % 8) ^ 5)) % 8).toInt
    val newA   = a / 8
    (newA, output)
  }

  case class State(
    a: N,
    b: N,
    c: N,
    program: List[Int],
    ip: Int = 0,
    output: List[Int] = Nil,
  ) {
    def explanation: String = {
      val instructions = program.grouped(2).toList
      val lines        = instructions.map {
        case List(instructionCode, comboCode) =>
          val instructionName = Instruction.fromInt(instructionCode)
          instructionName.debug(comboCode)
        case _                                =>
          "Unexpected".fail
      }

      lines.mkString("\n")
    }

    def step: Either[State, State] =
      (program.lift(ip), program.lift(ip + 1)) match {
        case (Some(instructionCode), Some(comboCode)) =>
          applyInstruction(
            Instruction.fromInt(instructionCode),
            comboCode,
          ).asRight
        case _                                        =>
          this.asLeft
      }

    private def combo(comboCode: Int): N =
      comboCode match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => 3
        case 4 => a
        case 5 => b
        case 6 => c
        case 7 => s"Invalid combo 7".fail
      }

    private def jumpIp: State = copy(ip = ip + 2)

    private def applyInstruction(
      instruction: Instruction,
      comboOrLiteralCode: Int,
    ): State = {
      def dv: (Long, State) = {
        val numerator   = a
        val denominator = pow(2, combo(comboOrLiteralCode).toInt)
        val result      = numerator / denominator
        (result, jumpIp)
      }

      instruction match {
        case Instruction.Adv =>
          val (toWrite, newState) = dv
          newState.copy(a = toWrite)

        case Instruction.Bxl =>
          val result = b ^ comboOrLiteralCode
          copy(b = result).jumpIp

        case Instruction.Bst =>
          val result = combo(comboOrLiteralCode) % 8
          copy(b = result).jumpIp

        case Instruction.Jnz =>
          if (a != 0) {
            copy(ip = comboOrLiteralCode)
          } else {
            jumpIp
          }

        case Instruction.Bxc =>
          val result = b ^ c
          copy(b = result).jumpIp

        case Instruction.Out =>
          val result = (combo(comboOrLiteralCode) % 8).toInt
          copy(output = result :: output).jumpIp

        case Instruction.Bdv =>
          val (toWrite, newState) = dv
          newState.copy(b = toWrite)

        case Instruction.Cdv =>
          val (toWrite, newState) = dv
          newState.copy(c = toWrite)
      }
    }
  }

  private def explainCombo(comboCode: Int): String =
    comboCode match {
      case 0 => "0"
      case 1 => "1"
      case 2 => "2"
      case 3 => "3"
      case 4 => "a"
      case 5 => "b"
      case 6 => "c"
      case 7 => "7"
      case _ => s"Unknown combo code $comboCode".fail
    }

  sealed trait Instruction {
    def debug(comboCode: Int): String = this match {
      case Instruction.Adv => s"a = a / (2 pow ${explainCombo(comboCode)})"
      case Instruction.Bxl => s"b = b ^ $comboCode"
      case Instruction.Bst => s"b = ${explainCombo(comboCode)} % 8"
      case Instruction.Jnz => s"if (a != 0) Jump $comboCode"
      case Instruction.Bxc => s"b = b ^ c"
      case Instruction.Out => s"Out ${explainCombo(comboCode)}"
      case Instruction.Bdv => s"Bdv ${explainCombo(comboCode)}"
      case Instruction.Cdv => s"c = a / (2 ^ ${explainCombo(comboCode)})"
    }
  }
  object Instruction       {
    case object Adv extends Instruction
    case object Bxl extends Instruction
    case object Bst extends Instruction
    case object Jnz extends Instruction
    case object Bxc extends Instruction
    case object Out extends Instruction
    case object Bdv extends Instruction
    case object Cdv extends Instruction

    def fromInt(i: Int): Instruction = i match {
      case 0 => Adv
      case 1 => Bxl
      case 2 => Bst
      case 3 => Jnz
      case 4 => Bxc
      case 5 => Out
      case 6 => Bdv
      case 7 => Cdv
      case _ => s"Unknown instruction: $i".fail
    }
  }

  def getOutput(data: Input): List[Int] = {
    val results = Simulation.runWithIterationCount(data) { case (state, _) =>
      state.step
    }

    results.output.reverse
  }

  def part1(data: Input): Output =
    getOutput(data).mkString(",")

  def part2(): Long = {
    println(RealData.explanation)
    solver(RealData.program)
  }

  def solver(data: List[Int]): Long = {
    /*
      f(a15) = (a14, 2)
      f(a14) = (a13, 4)
      f(a13) = (a12, 1)
      ...
      f(a1) = (0, 0)

      find min a15 such that all such is true
     */
    def search(a: Long, expectations: List[Int]): Option[Long] = {
      def reverseF(aNew: Long, output: Int): IndexedSeq[Long] = {
        val EmpiricConstant = 10000
        val results         = (aNew * 8 to aNew * 8 + EmpiricConstant).filter {
          candidate =>
            val (newA, out) = f(candidate)
            out == output && newA == aNew
        }

        println(
          s"Found ${results.length} candidates for $aNew, $output: $results"
        )
        results
      }

      expectations match {
        case Nil    => a.some
        case h :: t =>
          reverseF(a, h).foreach { candidate =>
            search(candidate, t) match {
              case Some(answer) =>
                return answer.some
              case None         =>
            }
          }

          None
      }
    }

    search(0, data.reverse).getOrElse("No solution found".fail)
  }

  val RealData: State =
    State(44374556, 0, 0, List(2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0))

  def main(args: Array[String]): Unit = {
    println(s"Part 1: ${part1(RealData)}")
    println(s"Part 2: ${part2()}")
  }
}
