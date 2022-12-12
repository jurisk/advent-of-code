package jurisk.adventofcode.y2022

import jurisk.utils.Parsing.{PrefixRemover, StringOps}
import jurisk.utils.FileInput._
import jurisk.geometry.{Coords2D, Field2D}
import org.scalatest.matchers.should.Matchers._

object Advent10 {
  type Parsed = List[Op]

  sealed trait Op
  object Op {
    case object Noop                             extends Op
    case class AddX(cyclesLeft: Int, value: Int) extends Op

    def parse(s: String): Op = {
      val AddXPrefix = PrefixRemover("addx ")

      s match {
        case "noop"        => Noop
        case AddXPrefix(n) => AddX(2, n.toInt)
        case _             => sys.error(s"Failed to parse $s")
      }
    }
  }

  def parse(data: String): Parsed =
    data.parseList("\n", Op.parse)

  case class State(x: Int, stack: List[Op]) {
    def next: State =
      stack match {
        case Op.Noop :: tail                   =>
          State(
            x = x,
            stack = tail,
          )
        case Op.AddX(1, value) :: tail         =>
          State(
            x = x + value,
            stack = tail,
          )
        case Op.AddX(turnsLeft, value) :: tail =>
          State(
            x = x,
            stack = Op.AddX(turnsLeft - 1, value) :: tail,
          )
        case Nil                               => this
      }
  }

  def part1(data: Parsed, relevantCycles: Set[Int]): Int = {
    val Width                 = 40
    val Height                = 6
    var screen: Field2D[Char] = Field2D.ofSize(Width, Height, '.')

    var state               = State(x = 1, stack = data)
    var totalSignalStrength = 0

    (0 until Width * Height) foreach { idx =>
      val x = idx % Width
      val y = idx / Width

      if (Math.abs(state.x - x) <= 1) {
        screen = screen.updatedAtUnsafe(Coords2D.of(x, y), '#')
      }

      val cycle = idx + 1

      if (relevantCycles.contains(cycle)) {
        println(s"cycle = $cycle, state = $state")
        val signalStrength = cycle * state.x
        totalSignalStrength += signalStrength
      }

      state = state.next
    }

    println(Field2D.toDebugRepresentation(screen))

    totalSignalStrength
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/10-test.txt")
    val realData = readFileText("2022/10.txt")

    val smallTest = parse("noop\naddx 3\naddx -5")
    part1(smallTest, Set(1, 2, 3, 4, 5, 6)) shouldEqual 36

    val test = parse(testData)
    val real = parse(realData)

    val RelevantCycles = Set(20, 60, 100, 140, 180, 220)

    part1(test, RelevantCycles) shouldEqual 13140
    part1(real, RelevantCycles) shouldEqual 15680
  }
}
