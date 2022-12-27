package jurisk.adventofcode.y2018

import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

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
      case Instruction.SetR(a, _, c) =>
        f"$idx%2d: $instruction        r$c = r$a"
      case Instruction.SetI(a, _, c) =>
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

object Program {
  def parse(data: String): Program = {
    val lines      = data.split("\n")
    val ipRegister = lines.head match {
      case s"#ip $ip" => ip.toInt
      case s          => s.failedToParse
    }

    val instructions = lines.tail.map(Instruction.parse)
    Program(ipRegister, ArraySeq.from(instructions))
  }
}

final case class Registers(data: ArraySeq[Int]) {
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
  def empty(size: Int): Registers = Registers(ArraySeq.fill(size)(0))
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

case object Instruction {
  type InstructionFactoryF = (Int, Int, Int) => Instruction

  val AllInstructions: Map[InstructionName, InstructionFactoryF] =
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
      case _                 => s.failedToParse
    }
}
