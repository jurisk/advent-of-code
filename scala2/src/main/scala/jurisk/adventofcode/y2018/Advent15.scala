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

  def part1(initial: State): (State, Int) = {
    val state = Simulation.run(initial) { state =>
      state.debugPrint()

      state.nextRound match {
        case Some(newState) => newState.asRight
        case None           => state.asLeft
      }
    }

    (state, state.roundsCompleted * state.hitPointsRemaining)
  }

  def part2(initial: State): String =
    initial.toString

  def main(args: Array[String]): Unit = {
    val realData = readFileText("2018/15.txt")

    val real = parse(realData)

    part1(real) shouldEqual 12345678
  }
}
