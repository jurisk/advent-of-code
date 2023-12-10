package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.algorithms.search.BinarySearch.binarySearchForLowestValidN
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent24 {
  final case class AttackType(value: String)

  object AttackType {
    private val validAttacks: Set[String] = Set(
      "fire",
      "bludgeoning",
      "radiation",
      "fire",
      "slashing",
      "cold",
    )

    def parseSet(s: String): Set[AttackType] =
      s.split(", ").toSet.map(AttackType.parse)
    def parse(s: String): AttackType         =
      if (validAttacks.contains(s)) AttackType(s) else s"Invalid attack $s".fail
  }

  final case class Group(
    id: Group.Id,
    units: Int,
    hitPointsPerUnit: Int,
    immuneTo: Set[AttackType],
    weakTo: Set[AttackType],
    attackDamage: Int,
    attackType: AttackType,
    initiative: Int,
  ) {
    def effectivePower: Int = units * attackDamage

    def theoreticalDamage(against: Group): Int = {
      val multiplier = if (against.immuneTo.contains(attackType)) {
        0
      } else if (against.weakTo.contains(attackType)) {
        2
      } else {
        1
      }

      effectivePower * multiplier
    }
  }

  object Group {
    type Id = String

    def groupAttacksGroup(attacker: Group, defender: Group): Option[Group] = {
      val theoreticalDamage = attacker.theoreticalDamage(defender)
      val unitsKilled       =
        Math.min(theoreticalDamage / defender.hitPointsPerUnit, defender.units)

      // println(s"${attacker.id} attacks ${defender.id}, killing $unitsKilled units (theoretical damage was $theoreticalDamage)")
      if (unitsKilled >= defender.units) {
        none
      } else {
        defender.copy(units = defender.units - unitsKilled).some
      }
    }

    def parse(s: String, id: Group.Id): Group =
      s match {
        case s"$units units each with $hitPoints hit points with an attack that does $attack $attackType damage at initiative $initiative" =>
          Group(
            id,
            units.toInt,
            hitPoints.toInt,
            Set.empty,
            Set.empty,
            attack.toInt,
            AttackType.parse(attackType),
            initiative.toInt,
          )

        case s"$units units each with $hitPoints hit points ($weaknessesOrImmunities) with an attack that does $attack $attackType damage at initiative $initiative" =>
          val (immuneTo, weakTo): (Set[AttackType], Set[AttackType]) =
            weaknessesOrImmunities match {
              case s"immune to $immunities; weak to $weaknesses" =>
                (
                  AttackType.parseSet(immunities),
                  AttackType.parseSet(weaknesses),
                )
              case s"weak to $weaknesses; immune to $immunities" =>
                (
                  AttackType.parseSet(immunities),
                  AttackType.parseSet(weaknesses),
                )
              case s"immune to $immunities"                      =>
                (AttackType.parseSet(immunities), Set.empty)
              case s"weak to $weaknesses"                        =>
                (Set.empty, AttackType.parseSet(weaknesses))
              case _                                             => weaknessesOrImmunities.failedToParse
            }
          Group(
            id,
            units.toInt,
            hitPoints.toInt,
            immuneTo,
            weakTo,
            attack.toInt,
            AttackType.parse(attackType),
            initiative.toInt,
          )

        case s => s.failedToParse
      }
  }

  final case class State(
    immuneSystem: Map[Group.Id, Group],
    infection: Map[Group.Id, Group],
  ) {
    def boostImmuneSystem(n: Int): State =
      copy(
        immuneSystem = immuneSystem map { case (id, group) =>
          id -> group.copy(attackDamage = group.attackDamage + n)
        }
      )

    override def toString: String = {
      def units(data: Map[Group.Id, Group]): String =
        data.values
          .map(g => s"${g.id} contains ${g.units} units")
          .toList
          .sorted
          .mkString("\n")

      s"""
         |Immune System:
         |${units(immuneSystem)}
         |
         |Infection:
         |${units(infection)}
         |
         |""".stripMargin
    }

    private def groupAttacksTarget(
      attackerId: Group.Id,
      defenderId: Group.Id,
    ): State = {
      def f(
        attackerArmy: Map[Group.Id, Group],
        defenderArmy: Map[Group.Id, Group],
      ): Map[Group.Id, Group] =
        defenderArmy.updatedWith(defenderId)(maybeFound =>
          maybeFound.flatMap { found =>
            attackerArmy.get(attackerId) match {
              case None           => found.some
              case Some(attacker) => Group.groupAttacksGroup(attacker, found)
            }
          }
        )

      copy(
        immuneSystem = f(infection, immuneSystem),
        infection = f(immuneSystem, infection),
      )
    }

    def next: State = {
      // During the target selection phase, each group attempts to choose one target.
      val immuneSystemTargets: Map[Group.Id, Group.Id] =
        State.selectTargets(immuneSystem, infection)
      val infectionTargets: Map[Group.Id, Group.Id]    =
        State.selectTargets(infection, immuneSystem)

      val mergedTargets =
        immuneSystemTargets ++ infectionTargets // relies that IDs are globally unique

      // At the end of the target selection phase, each group has selected zero or one groups to attack, and each group
      // is being attacked by zero or one groups.

      // Groups attack in decreasing order of initiative, regardless of whether they are part of the infection or the
      // immune system. (If a group contains no units, it cannot attack.)
      val groupsInAttackOrder: List[Group.Id] =
        (immuneSystem.values.toSet ++ infection.values.toSet).toList
          .sortBy(_.initiative)(Ordering[Int].reverse)
          .map(_.id)

      groupsInAttackOrder.foldLeft(this) { case (acc, groupId) =>
        val target = mergedTargets.get(groupId)
        target match {
          case Some(target) => acc.groupAttacksTarget(groupId, target)
          case None         => acc
        }
      }
    }
  }

  object State {
    private def selectTarget(by: Group, against: List[Group]): Option[Group] = {
      // The attacking group chooses to target the group in the enemy army to which it would deal the most damage (after
      // accounting for weaknesses and immunities, but not accounting for whether the defending group has enough units
      // to actually receive all of that damage).

      // If an attacking group is considering two defending groups to which it would deal equal damage,
      // it chooses to target the defending group with the largest effective power; if there is still a tie, it chooses
      // the defending group with the highest initiative.
      val result = against.maxByOption(g =>
        (
          by.theoreticalDamage(g),
          g.effectivePower,
          g.initiative,
        )
      )

      result match {
        // If it cannot deal any defending groups damage, it does not choose a target.
        case Some(result) =>
          if (by.theoreticalDamage(result) <= 0) none else result.some
        case None         => None
      }
    }

    private def selectTargets(
      by: Map[Group.Id, Group],
      against: Map[Group.Id, Group],
    ): Map[Group.Id, Group.Id] = {
      var results: Map[Group.Id, Group.Id] = Map.empty

      //  In decreasing order of effective power, groups choose their targets; in a tie, the group with the higher
      //  initiative chooses first.
      val orderedInAttackOrder = by.values.toList.sortBy(g =>
        (g.effectivePower, g.initiative)
      )(Ordering[(Int, Int)].reverse)
      orderedInAttackOrder foreach { group =>
        val notYetAttacked = against.values
          .filterNot(a => results.values.toSet.contains(a.id))
          .toList
        val selected       = selectTarget(group, notYetAttacked)
        selected foreach { selected =>
          results = results.updated(group.id, selected.id)
        }
      }

      results
    }

    def parse(s: String): State = {
      val List(immuneSystem, infection) = s.splitByDoubleNewline

      def parseGroupSet(
        s: String,
        header: String,
        idPrefix: String,
      ): Map[Group.Id, Group] = {
        val lines = s.split("\n")
        lines.head shouldEqual header

        lines.tail.zipWithIndex.map { case (x, idx) =>
          val id = s"$idPrefix${idx + 1}"
          id -> Group.parse(x, id)
        }.toMap
      }

      State(
        parseGroupSet(immuneSystem, "Immune System:", "Immune System group "),
        parseGroupSet(infection, "Infection:", "Infection group "),
      )
    }
  }

  private def simulateRound(state: State): (Int, Int) = {
    val (result, _) = Simulation.runUntilStableState(state) { case (state, _) =>
      state.next
    }

    (
      result.immuneSystem.values.map(_.units).sum,
      result.infection.values.map(_.units).sum,
    )
  }

  def part1(state: State): Int = {
    val (immuneSystemUnits, infectionUnits) = simulateRound(state)
    immuneSystemUnits + infectionUnits
  }

  // Some(unitsLeft) if Immune System wins, None otherwise
  private def hypotheticalBoost(state: State, n: Int): Option[Int] = {
    val boosted                             = state.boostImmuneSystem(n)
    val (immuneSystemUnits, infectionUnits) = simulateRound(boosted)
    if (infectionUnits == 0) {
      immuneSystemUnits.some
    } else {
      none
    }
  }

  def part2(state: State): Int = {
    // Find lowest `n` so that `state.boostImmuneSystem(n)` has Immune System win.
    // How many units is it left with?
    val (_, units) =
      binarySearchForLowestValidN(0, 1, n => hypotheticalBoost(state, n))
    units
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/24-test.txt")
    val realData = readFileText("2018/24.txt")

    val test = State.parse(testData)
    val real = State.parse(realData)

    part1(test) shouldEqual 5216
    part1(real) shouldEqual 29865

    hypotheticalBoost(test, 1570) shouldEqual Some(51)
    part2(real) shouldEqual 2444
  }
}
