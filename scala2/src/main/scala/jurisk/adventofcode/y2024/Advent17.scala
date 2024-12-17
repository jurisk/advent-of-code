package jurisk.adventofcode.y2024

import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId, none}
import jurisk.math.pow
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation

object Advent17 {
  type Input  = State
  type Output = String
  type N      = Long

  @inline
  def fastPow2(n: Long): N = {
    1L << n
//    val result = 1L << n
//    assert(result == pow(2, n.toInt))
//    result
  }

  def calculateOutput(a: Long): Long = {
    (a % 8) ^ 0b011 ^ (a / fastPow2((a % 8) ^ 0b101)) % 8
  }

  def findA(output: Int): Long = {
    var candidateA = 0
    while (candidateA < Long.MaxValue) {
      if (calculateOutput(candidateA) == output) {
        return candidateA
      }
      candidateA += 1
    }
    "asdf".fail
  }

  /* a => (new_a, output) */
  def f(a: Long): (Long, Int) = {
    val output = ((a % 8) ^ 0b011 ^ (a / fastPow2((a % 8) ^ 0b101)) % 8).toInt
    val newA = a / 8
    (newA, output)
  }

  def run(initialA: N, expected: Array[Int]): Boolean = {
    var a: N = initialA
    var expectedIdx = 0

    while (a != 0) {
      val output = (a % 8) ^ 0b011 ^ (a / fastPow2((a % 8) ^ 0b101)) % 8
      println(s"output = $output")
      if (expected(expectedIdx) != output) {
        return false
      } else {
        expectedIdx += 1
        if (expectedIdx > 10) {
          println(s"Output $expectedIdx for $initialA ${BigInt(initialA).toString(3)}")
        }
        if (expectedIdx == expected.length) {
          return true
        }
      }

      println(s"$a => ${a / 8}")
      a = a / 8
    }

    expectedIdx == expected.length
  }

  case class State(
    a: N,
    b: N,
    c: N,
    program: List[Int],
    ip: Int = 0,
    output: List[Int] = Nil,
    expectedOutput: List[Int] = Nil,
  ) {
    def step: Either[State, State] = {
      (program.lift(ip), program.lift(ip + 1)) match {
        case (Some(instructionCode), Some(comboCode)) =>
          applyInstruction(
            Instruction.fromInt(instructionCode),
            comboCode,
          ) match {
            case (newState, res) =>
              res match {
                case Some(res) =>
                  val qq = newState.copy(output = res :: output)
                  if (expectedOutput.headOption.contains(res)) {
                    val t = expectedOutput.tail
                    if (t.isEmpty) {
                      qq.asLeft
                    } else {
                      qq.copy(expectedOutput = t).asRight
                    }
                  } else {
                    qq.asLeft
                  }
                case None =>
                  newState.asRight
              }

          }
        case _                                        =>
          this.asLeft
      }
    }

    def combo(comboCode: Int): N =
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

    def jumpIp: State = copy(ip = ip + 2)

    def applyInstruction(
      instruction: Instruction,
      comboOrLiteralCode: Int,
    ): (State, Option[Int]) = {
      instruction match {
        case Instruction.Out =>
          val result: Int = (combo(comboOrLiteralCode) % 8).toInt
          (jumpIp, result.some)

        case other =>
          val result = applyInstruction2(other, comboOrLiteralCode)
          (result, none)
      }
    }

    def applyInstruction2(
      instruction: Instruction,
      comboOrLiteralCode: Int,
    ): State =
      instruction match {
        case Instruction.Adv =>
          val numerator   = a
          val denominator = pow(2, combo(comboOrLiteralCode).toInt)
          val result      = numerator / denominator
          copy(a = result).jumpIp
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
          val numerator   = a
          val denominator = pow(2, combo(comboOrLiteralCode).toInt)
          val result      = numerator / denominator
          copy(b = result).jumpIp

        case Instruction.Cdv =>
          val numerator   = a
          val denominator = pow(2, combo(comboOrLiteralCode).toInt)
          val result      = numerator / denominator
          copy(c = result).jumpIp
      }
  }

  def explainCombo(comboCode: Int): String = {
    comboCode match {
      case 0 => "0"
      case 1 => "1"
      case 2 => "2"
      case 3 => "3"
      case 4 => "a"
      case 5 => "b"
      case 6 => "c"
      case 7 => "7"
      case _ => "asdf".fail
    }
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
  object Instruction {
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

  def part1(data: Input): Output = {
    getOutput(data).mkString(",")
  }

  /*

a = x
b = 0
c = 0

b = (a % 8) ^ 5
c = a / (2 ^ b)
b = b ^ 6
a = a / (2 ^ 3)
b = b ^ c
Out b
if (a != 0) Jump Start

// Should output: 2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0

   */

  def part2(data: Input): Long = {
    data.program.zipWithIndex.grouped(2).foreach {
      case List((a, aIdx), (b, bIdx)) =>
        println(s"$aIdx: ${Instruction.fromInt(a).debug(b)}")
      case _ => "asdf".fail
    }

    val N = 5
    val rray = data.program.take(N).toArray // .take(n)
//    var candidateA: Long = 0 // 108400000000L // 0
//    while (candidateA < Long.MaxValue) {
//      if (candidateA % 100_000_000 == 0) {
//        println(s"Trying $candidateA")
//      }
//      if (run(candidateA, rray)) {
//        return candidateA
//      }
//        candidateA += 1
//    }
//    "asdf".fail

    var candidateA = 0
    while (candidateA < Long.MaxValue) {
      if (candidateA % 1_000_000 == 0) {
        println(s"Trying $candidateA")
      }
      val adjusted = data.copy(a = candidateA, expectedOutput = data.program.take(N))
      val output = getOutput(adjusted)

      if (output.length > 8) {
        println(s"Output for $candidateA ${BigInt(candidateA).toString(3)}: $output")
      }
      if (output == data.program) {
        return candidateA
      }
      candidateA += 1
    }
    "asdf".fail
  }

  val RealData: State =
    State(44374556, 0, 0, List(2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0))

  def main(args: Array[String]): Unit = {
    (0 to 1000) foreach { a =>
      println(s"$a: ${calculateOutput(a)}")
    }

    (0 to 7) foreach { output =>
      println(s"To get $output a has to be ${findA(output)}")
    }

//    println(s"Part 1: ${part1(RealData)}")
    println(s"Part 2: ${part2(RealData)}")
  }
}
