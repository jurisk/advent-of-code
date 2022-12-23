package jurisk.adventofcode.y2018

import cats.implicits.catsSyntaxEitherId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

object Advent19 {
  final case class Program(
    ipRegister: Int,
    instructions: ArraySeq[Instruction],
  ) {
    private def debugInstruction(idx: Int): String = {
      val instruction = instructions(idx)
      val raw         = instruction match {
        case Instruction.AddR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a + r$b"
        case Instruction.AddI(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a + $b"
        case Instruction.MulR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a * r$b"
        case Instruction.MulI(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a * $b"
        case Instruction.BAnR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a & r$b"
        case Instruction.BAnI(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a & $b"
        case Instruction.BOrR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a | r$b"
        case Instruction.BOrI(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a | $b"
        case Instruction.SetR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = r$a"
        case Instruction.SetI(a, b, c) =>
          f"$idx%2d: $instruction        r$c = $a"
        case Instruction.GtRR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = if (r$a > r$b) 1 else 0"
        case Instruction.EqRR(a, b, c) =>
          f"$idx%2d: $instruction        r$c = if (r$a == r$b) 1 else 0"
        case _                         =>
          f"$idx%2d: $instruction        not supported as not present in program"
      }

      raw.replace(
        s"r$ipRegister",
        "ip",
      ) // actually all jumps are to the preceding instruction
    }

    def debugPrint(): Unit = {
      instructions.indices.foreach { idx =>
        val line = debugInstruction(idx)
        println(line)
      }
      println()
    }

    def next(registers: Registers): Option[Registers] =
      instructions.lift(registers(ipRegister)) map { instruction =>
        instruction.execute(registers).incrementRegister(ipRegister)
      }
  }

  final case class Registers(data: ArraySeq[Int]) {
    require(data.length == 6)

    def apply(index: Int): Int =
      data(index)

    def setRegister(index: Int, value: Int): Registers =
      Registers(
        data.updated(index, value)
      )

    def incrementRegister(index: Int): Registers =
      setRegister(index, data(index) + 1)

    override def toString: String = data.mkString("[", ", ", "]")
  }

  object Registers {
    def apply(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): Registers =
      Registers(
        ArraySeq(a, b, c, d, e, f)
      )

    val Start: Registers = Registers(0, 0, 0, 0, 0, 0)
  }

  final case class InstructionName(name: String) extends AnyVal {
    override def toString: String = name
  }

  sealed trait Instruction {
    val a: Int
    val b: Int
    val c: Int

    override def toString: String =
      f"$instructionName $a%2d $b%2d $c%2d"

    def instructionName: InstructionName = InstructionName(
      getClass.getSimpleName.toLowerCase
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

    def parse(s: String): Instruction =
      s match {
        case s"$name $a $b $c" =>
          instructionFactoryByName(InstructionName(name))(
            a.toInt,
            b.toInt,
            c.toInt,
          )
      }
  }

  def parse(data: String): Program = {
    val lines      = data.split("\n")
    val ipRegister = lines.head match {
      case s"#ip $ip" => ip.toInt
    }

    val instructions = lines.tail.map(Instruction.parse)
    Program(ipRegister, ArraySeq.from(instructions))
  }

  def part1(program: Program): Int = {
    program.debugPrint()

    val result = Simulation.runWithIterationCount(Registers.Start) {
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
      Simulation.runWithIterationCount(Registers.Start.setRegister(0, 1)) {
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

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 7
    part1(real) shouldEqual 2280

    part2(real) shouldEqual 30481920
  }
}
