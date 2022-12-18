package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.adventofcode.y2018.Advent15.Race.{Elf, Goblin}
import jurisk.algorithms.pathfinding.Pathfinding
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

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
    val InitialHitPoints: Int = 200
  }

  final case class State(
    roundsCompleted: Int,
    walls: Field2D[Boolean],
    units: Map[WarriorId, Warrior],
    attackPower: Map[Race, Int],
  ) {
    def aliveOfRace(race: Race): Int =
      activeUnits.values.count(_.race == race)

    def updateAttackPower(race: Race, power: Int): State =
      copy(attackPower = attackPower.updated(race, power))

    def debugRepresentation: String = {
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
          s"$a   $b".trim
        }

      result.mkString("\n")
    }

    def debugPrint(): Unit = {
      println(s"After $roundsCompleted rounds:")
      println(debugRepresentation)
    }

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
      val target        = chooseTarget(targets)
      val hitPointsLost = attackPower(target.race.enemyRace)
      val newHitPoints  = Math.max(target.hitPoints - hitPointsLost, 0)

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

        val result = warriorIdsInOrder
          .foldLeft(this) { case (acc, warriorId) =>
            acc.performTurn(warriorId)
          }

        if (result != this) result.copy(roundsCompleted + 1).some
        else {
          println(s"This is happening, cannot remove")
          // TODO: if this is never happening we can remove this
          none
        }
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
      Map(Elf -> 3, Goblin -> 3),
    )
  }

  def part1(initial: State): (State, Int) = {
    val state = Simulation.run(initial) { state =>
      // state.debugPrint()

      state.nextRound match {
        case Some(newState) => newState.asRight
        case None           => state.asLeft
      }
    }

//    val hackedState = state.copy(roundsCompleted = state.roundsCompleted - 1) // No idea why the `- 1` is needed, but it works on everything except Test 1
//    (hackedState, hackedState.roundsCompleted * hackedState.hitPointsRemaining)
    (state, state.roundsCompleted * state.hitPointsRemaining)
  }

  // low - Elves lose at this, high - Elves win at this
  @tailrec
  private def pivotSearch(initial: State, low: Int, high: Int): Int = {
    require(low < high)
    if (high - low == 1) {
      high
    } else {
      val mid    = (high + low) / 2
      val result = doElvesWinWithoutAnyLosses(initial, mid)
      result match {
        case Some(_) => pivotSearch(initial, low, mid)
        case None    => pivotSearch(initial, mid, high)
      }
    }
  }

  def part2(initial: State): Int = {
    var min                   = 3
    require(doElvesWinWithoutAnyLosses(initial, min).isEmpty)
    var max                   = 9
    while (doElvesWinWithoutAnyLosses(initial, max).isEmpty) {
      min = max
      max *= 2
    }
    val foundMinNoLossWinning = pivotSearch(initial, min, max)
    println(s"Found min no loss winning: $foundMinNoLossWinning")

    // Here we run it again though we could have avoided it
    doElvesWinWithoutAnyLosses(initial, foundMinNoLossWinning).get._2
  }

  // None - Elves lose, Some - Elves win with some finish State & score
  private def doElvesWinWithoutAnyLosses(
    initial: State,
    elvenPower: Int,
  ): Option[(State, Int)] = {
    println(s"Running with elven power $elvenPower")

    val adjusted          = initial.updateAttackPower(Elf, elvenPower)
    val initialElvenCount = initial.aliveOfRace(Elf)

    val result: Option[State] = Simulation.run(adjusted) { state =>
      // state.debugPrint()

      state.nextRound match {
        case Some(newState) =>
          val currentElvenCount = newState.aliveOfRace(Elf)
          if (currentElvenCount < initialElvenCount) { // an Elf has died
            none.asLeft
          } else {
            newState.asRight
          }

        case None => state.some.asLeft
      }
    }

    if (result.isEmpty) {
      println(s"The Elves lost, elven power $elvenPower was insufficient")
    }

    result.map { state =>
//      val hackedState = state.copy(roundsCompleted = state.roundsCompleted - 1) // No idea why the `- 1` is needed, but it works on everything except Test 1
//      val hackedScore = hackedState.roundsCompleted * hackedState.hitPointsRemaining
      val score = state.roundsCompleted * state.hitPointsRemaining
      println(
        s"The Elves won, elven power $elvenPower was sufficient, score $score"
      )
//       (hackedState, hackedScore)
      (state, score)
    }
  }

  def main(args: Array[String]): Unit = {
    val realData = readFileText("2018/15.txt")

    val real = parse(realData)

    part1(real)._2 shouldEqual 243390
    part2(real) shouldEqual "todo" // TODO; 50184 is too low
  }
}
