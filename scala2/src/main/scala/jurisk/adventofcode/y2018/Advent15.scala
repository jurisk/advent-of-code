package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.algorithms.pathfinding.Pathfinding
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent15 {
  final case class WarriorId(initialPosition: Coords2D) extends AnyVal
  final case class Warrior(
    id: WarriorId,
    location: Coords2D,
    race: Race,
    hitPoints: Int,
  ) {
    def isAlive: Boolean = hitPoints > 0
    def toChar: Char     = if (isAlive) race.toChar else '.'
  }

  // When multiple choices are equally valid, ties are broken in reading order: top-to-bottom, then left-to-right.
  implicit private val coordsOrdering: Ordering[Coords2D] =
    Ordering[(Int, Int)].contramap(c => (c.y.value, c.x.value))

  object Warrior {
    val AttackPower: Int      = 3
    val InitialHitPoints: Int = 200
  }

  final case class State(
    roundsCompleted: Int,
    walls: Field2D[Boolean],
    units: Map[WarriorId, Warrior],
  ) {
    def debugRepresentation: String = {
      val heading   = s"After $roundsCompleted rounds"
      val charField = walls.mapByCoordsWithValues { case (c, w) =>
        units.values.filter(q => q.location == c && q.isAlive).toList match {
          case Nil            => if (w) '#' else '.'
          case warrior :: Nil => warrior.toChar
          case more           =>
            s"Did not expect multiple warriors in one square $c: $more".fail
        }
      }
      val fieldInfo = Field2D.toDebugRepresentation(charField).split('\n')

      val unitInfo = walls.yIndices map { y =>
        units.values
          .filter(_.location.y == y)
          .filter(_.isAlive)
          .toList
          .sortBy(_.location.x.value)
          .map(w => s"${w.race.toChar}(${w.hitPoints})")
          .mkString(", ")
      }

      require(fieldInfo.length == unitInfo.length)

      val result = (fieldInfo zip unitInfo).toList
        .map { case (a, b) =>
          s"$a $b"
        }

      (heading :: result).mkString("\n")
    }

    def debugPrint(): Unit =
      println(debugRepresentation)

    private def isFree(c: Coords2D): Boolean =
      walls.at(c).contains(false) && !units.values.exists(w =>
        w.location == c && w.isAlive
      )

    private def activeUnits: Map[WarriorId, Warrior] = units.filter {
      case (_, v) => v.isAlive
    }

    private def chooseTarget(targets: Iterable[Warrior]): Warrior = {
      val minHitPoints     = targets.map(_.hitPoints).min
      val withMinHitpoints = targets.filter(_.hitPoints == minHitPoints)
      withMinHitpoints.minBy(_.location)
    }

    def setUnitHitPoints(warriorId: WarriorId, newHitPoints: Int): State =
      setUnitHitPoints(units(warriorId), newHitPoints)

    def setUnitHitPoints(warrior: Warrior, newHitPoints: Int): State =
      copy(
        units =
          units.updated(warrior.id, warrior.copy(hitPoints = newHitPoints))
      )

    def setUnitLocation(warrior: Warrior, newLocation: Coords2D): State =
      copy(
        units = units.updated(warrior.id, warrior.copy(location = newLocation))
      )

    private def performAttack(targets: Iterable[Warrior]): State = {
      val target       = chooseTarget(targets)
      val newHitPoints = Math.max(target.hitPoints - Warrior.AttackPower, 0)

      setUnitHitPoints(target, newHitPoints)
    }

    private def findSquaresAdjacentToTargets(
      targets: Iterable[Warrior]
    ): List[Coords2D] =
      targets.flatMap(_.location.adjacent4).filter(isFree).toList

    type Distance = Int

    private def findDistance(from: Coords2D, to: Coords2D): Option[Distance] =
      Pathfinding.shortestPathLength[Coords2D](
        from,
        x => walls.adjacent4(x).filter(isFree),
        _ == to,
      )

    private def findDistancesToMany(
      from: Coords2D,
      to: Iterable[Coords2D],
    ): List[(Coords2D, Distance)] =
      to.toList flatMap { to =>
        findDistance(from, to).map(to -> _)
      }

    private def findDistancesFromMany(
      from: Iterable[Coords2D],
      to: Coords2D,
    ): List[(Coords2D, Distance)] =
      from.toList flatMap { from =>
        findDistance(from, to).map(from -> _)
      }

    private def chooseBestSquare(
      optionsWithDistances: List[(Coords2D, Distance)]
    ): Option[Coords2D] =
      if (optionsWithDistances.isEmpty) None
      else {
        val minDistance = optionsWithDistances.map(_._2).min
        val withMin     = optionsWithDistances.filter(_._2 == minDistance).map(_._1)
        withMin.min.some
      }

    private def takeStepTowards(
      warrior: Warrior,
      destination: Coords2D,
    ): State = {
      val neighbours    = walls.adjacent4(warrior.location).filter(isFree)
      val withDistances = findDistancesFromMany(neighbours, destination)

      chooseBestSquare(withDistances) match {
        case Some(chosen) =>
          setUnitLocation(warrior, chosen)

        case None =>
          this
      }
    }

    private def attackIfPossible(warriorId: WarriorId): State = {
      val self                    = units(warriorId)
      val targetsDirectlyAdjacent = activeUnits.values
        .filter(_.race == self.race.enemyRace)
        .filter(_.location.manhattanDistance(self.location) == 1)
      if (targetsDirectlyAdjacent.nonEmpty) {
        performAttack(targetsDirectlyAdjacent)
      } else {
        this
      }
    }

    private def performTurn(warriorId: WarriorId): State = {
      val self = units(warriorId)
      if (self.isAlive) {
        // Each unit begins its turn by identifying all possible targets (enemy units).
        val targets = activeUnits.values.filter(_.race == self.race.enemyRace)
        if (targets.isEmpty) {
          this // If no targets remain, combat ends.
        } else {
          val targetsDirectlyAdjacent =
            targets.filter(_.location.manhattanDistance(self.location) == 1)
          if (targetsDirectlyAdjacent.nonEmpty) {
            performAttack(targetsDirectlyAdjacent)
          } else {
            val squaresAdjacentToTargets              = findSquaresAdjacentToTargets(targets)
            val squaresAdjacentToTargetsWithDistances =
              findDistancesToMany(self.location, squaresAdjacentToTargets)
            chooseBestSquare(squaresAdjacentToTargetsWithDistances) match {
              case Some(chosenSquare) =>
                takeStepTowards(self, chosenSquare).attackIfPossible(warriorId)
              case None               =>
                this
            }
          }
        }
      } else this
    }

    def hitPointsRemaining: Int = units.values.map(_.hitPoints).sum

    def nextRound: Option[State] =
      if (activeUnits.values.map(_.race).toSet.size < 2) {
        none // combat is over
      } else {
        val warriorIdsInOrder = activeUnits.keys.toList.sortBy { warriorId =>
          units(warriorId).location
        }
        warriorIdsInOrder
          .foldLeft(this) { case (acc, warriorId) =>
            acc.performTurn(warriorId)
          }
          .copy(
            roundsCompleted + 1
          )
          .some
      }
  }

  sealed trait Race {
    def toChar: Char
    def enemyRace: Race
  }

  object Race {
    case object Goblin extends Race {
      def toChar: Char    = 'G'
      def enemyRace: Race = Elf
    }

    case object Elf extends Race {
      def toChar: Char    = 'E'
      def enemyRace: Race = Goblin
    }
  }

  def parse(data: String): State = {
    val charField = Field2D.parseFromString(data, identity)
    val walls     = charField.map {
      case '#'             => true
      case '.' | 'G' | 'E' => false
      case ch              => s"Unexpected char '$ch'".fail
    }

    val units = charField.entries.flatMap { case (c, v) =>
      val race = v match {
        case 'E' => Race.Elf.some
        case 'G' => Race.Goblin.some
        case _   => none
      }

      race map { race =>
        val id = WarriorId(c)

        id -> Warrior(
          id = id,
          location = c,
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

  def part1(initial: State): Int =
    Simulation.run(initial) { state =>
      state.debugPrint()

      state.nextRound match {
        case Some(newState) => newState.asRight
        case None           => (state.roundsCompleted * state.hitPointsRemaining).asLeft
      }
    }

  def part2(initial: State): String =
    initial.toString

  private def usefulTestCase1(): Unit = {
    val test1After23Rounds = parse("""#######
                                     |#...G.#
                                     |#..G.G#
                                     |#.#.#G#
                                     |#...#E#
                                     |#.....#
                                     |#######
                                     |""".stripMargin)
      .copy(
        roundsCompleted = 23
      )
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 2)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 3)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 4)), 131)

    println("Test 1 After 23 rounds input:")
    test1After23Rounds.debugPrint()

    val test1After24RoundsExpected = parse("""#######
                                             |#..G..#
                                             |#...G.#
                                             |#.#G#G#
                                             |#...#E#
                                             |#.....#
                                             |#######""".stripMargin)
      .copy(
        roundsCompleted = 24
      )
      .setUnitHitPoints(WarriorId(Coords2D.of(4, 2)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 3)), 128)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 4)), 128)
    println("Test 1 After 24 rounds expected:")
    test1After24RoundsExpected.debugPrint()

    val test1After24RoundsObtained = test1After23Rounds.nextRound.get
    println("Test 1 After 24 rounds obtained:")
    test1After24RoundsObtained.debugPrint()

    test1After24RoundsObtained.debugRepresentation shouldEqual test1After24RoundsExpected.debugRepresentation
  }

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
    val real  = parse(realData)

    usefulTestCase1()

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
