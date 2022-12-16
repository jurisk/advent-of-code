package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent15 {
  final case class WarriorId(initialPosition: Coords2D) extends AnyVal
  final case class Warrior(id: WarriorId, position: Coords2D, race: Race, hitPoints: Int)

  object Warrior {
    val AttackPower: Int = 3
    val InitialHitPoints: Int = 200
  }

  final case class State(
    roundsCompleted: Int,
    walls: Field2D[Boolean],
    units: Map[WarriorId, Warrior],
  ) {
    def debugPrint(): Unit = {
      println(s"Rounds completed: $roundsCompleted")
      println(s"Units remaining: $units")
      val charField = walls.mapByCoordsWithValues { case (c, w) =>
        units.values.filter(_.position == c).toList match {
          case Nil => if (w) '#' else '.'
          case warrior :: Nil => warrior.race.toChar
          case more => s"Did not expect multiple warriors in one square $c: $more".fail
        }
      }
      println(Field2D.toDebugRepresentation(charField))
    }

    def nextUnitPositions: Option[Map[WarriorId, Warrior]] = {
      ???
    }

    def hitPointsRemaining: Int = units.values.map(_.hitPoints).sum

    def nextRound: Option[State] = {
      nextUnitPositions map { newUnitPositions =>
        State(
          roundsCompleted + 1,
          walls,
          newUnitPositions,
        )
      }
    }
  }

  sealed trait Race {
    def toChar: Char
  }
  object Race {
    case object Goblin extends Race {
      override def toChar: Char = 'G'
    }
    case object Elf extends Race {
      override def toChar: Char = 'E'
    }
  }

  def parse(data: String): State = {
    val charField = Field2D.parseFromString(data, identity)
    val walls = charField.map({
      case '#' => true
      case '.' | 'G' | 'E' => false
      case ch => s"Unexpected char '$ch'".fail
    })

    val units = charField.entries.flatMap { case (c, v) =>
      val race = v match {
        case 'E' => Race.Elf.some
        case 'G' => Race.Goblin.some
        case _ => none
      }

      race map { race =>
        val id = WarriorId(c)

        id -> Warrior(
          id = id,
          position = c,
          race = race,
          hitPoints = Warrior.InitialHitPoints,
        )
      }
    }.toMap

    State(
      0,
      walls,
      units,
    )
  }

  def part1(initial: State): Int = {
    initial.debugPrint()

    Simulation.run(initial) { state =>
      state.nextRound match {
        case Some(newState) => newState.asRight
        case None => (state.roundsCompleted * state.hitPointsRemaining).asLeft
      }
    }
  }

  def part2(initial: State): String =
    initial.toString

  def main(args: Array[String]): Unit = {
    val testData1 =
      """#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######""".stripMargin

    val testData2 =
      """#######
        |#G..#E#
        |#E#E.E#
        |#G.##.#
        |#...#E#
        |#...E.#
        |#######""".stripMargin

    val testData3 =
      """#######
        |#E..EG#
        |#.#G.E#
        |#E.##E#
        |#G..#.#
        |#..E#.#
        |#######""".stripMargin

    val testData4 =
      """#######
        |#E.G#.#
        |#.#G..#
        |#G.#.G#
        |#G..#.#
        |#...E.#
        |#######""".stripMargin

    val testData5 =
      """#######
        |#.E...#
        |#.#..G#
        |#.###.#
        |#E#G#G#
        |#...#G#
        |#######""".stripMargin

    val testData6 =
      """#########
        |#G......#
        |#.E.#...#
        |#..##..G#
        |#...##..#
        |#...#...#
        |#.G...G.#
        |#.....G.#
        |#########""".stripMargin

    val realData = readFileText("2018/15.txt")

    val test1 = parse(testData1)
    val test2 = parse(testData2)
    val test3 = parse(testData3)
    val test4 = parse(testData4)
    val test5 = parse(testData5)
    val test6 = parse(testData6)
    val real = parse(realData)

    part1(test1) shouldEqual 27730
    part1(test2) shouldEqual 36334
    part1(test3) shouldEqual 39514
    part1(test4) shouldEqual 27755
    part1(test5) shouldEqual 28944
    part1(test6) shouldEqual 18740

    part1(real) shouldEqual 12345678

    part2(test1) shouldEqual "asdf"
    part2(real) shouldEqual "asdf"
  }
}
