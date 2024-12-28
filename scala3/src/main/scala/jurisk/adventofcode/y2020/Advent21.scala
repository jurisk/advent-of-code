package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp
import jurisk.adventofcode.y2020.Advent21.Entry
import cats.implicits.*
import scala.annotation.tailrec

object Advent21 extends SingleLineAdventApp[Entry, Int, String]:
  override val year: Int = 2020
  override val exercise: Int = 21

  opaque type Allergen = String
  opaque type Ingredient = String

  final case class Entry(ingredients: Set[Ingredient], allergens: Set[Allergen])

  final case class Rules(entries: Set[Entry]):
    val ingredients: Set[Ingredient] = entries.flatMap(_.ingredients)
    val allergens: Set[Allergen] = entries.flatMap(_.allergens)

    def ingredientsWhichCanContain(allergen: Allergen): Set[Ingredient] =
      entries
        .filter(_.allergens.contains(allergen))
        .map(_.ingredients)
        .reduce { case (a, b) =>
          a intersect b
        }

  object Rules:
    def apply(list: List[Entry]): Rules = Rules(list.toSet)

  private def findCanonical(rules: Rules): Map[Allergen, Ingredient] =
    val simplified: Map[Allergen, Set[Ingredient]] = rules
      .allergens.map { allergen =>
        allergen -> rules.ingredientsWhichCanContain(allergen)
      }
      .toMap

    @tailrec
    def deduce(remaining: Map[Allergen, Set[Ingredient]], acc: Map[Allergen, Ingredient]): Map[Allergen, Ingredient] =
      if remaining.isEmpty then acc else
        val found = remaining.find { case (_, v) => v.size == 1 }

        found match
          case None => sys.error(s"Failed to find $remaining $acc")
          case Some((allergen, ingredientSet)) =>
            ingredientSet.toList match
              case ingredient :: Nil =>
                val newAcc = acc + (allergen -> ingredient)
                val newRemaining = (remaining - allergen).view.mapValues(x => x - ingredient).toMap
                deduce(newRemaining, newAcc)
              case _ => sys.error(s"Fail $ingredientSet")

    deduce(simplified, Map.empty)

  private def findSafe(rules: Rules, canonical: Map[Allergen, Ingredient]): Set[Ingredient] =
    val allergicIngredients = canonical.values.toSet
    rules.ingredients -- allergicIngredients

  private val RegEx = """(.+) \(contains (.+)\)""".r
  override def parseLine(line: String): Either[ErrorMessage, Entry] =
    line match
      case RegEx(ingredients, allergens) => Entry(
        ingredients.split(' ').toSet.map(x => new Ingredient(x)),
        allergens.split(", ").toSet.map(x => new Allergen(x)),
      ).asRight
      case _ => ErrorMessage.left(s"Couldn't parse $line")

  override def solution1(input: List[Entry]): Int =
    val rules = Rules(input)
    val canonical = findCanonical(rules)
    val safe = findSafe(rules, canonical)
    safe
      .toList
      .map { ingredient =>
        rules.entries.count(_.ingredients.contains(ingredient))
      }
      .sum


  override def solution2(input: List[Entry]): Ingredient =
    val rules = Rules(input)
    val canonical = findCanonical(rules)
    canonical
      .toList
      .sortBy { case (allergen, _) =>
        allergen
      }
      .map { case (_, ingredient) =>
        ingredient
      }
      .mkString(",")

