import scala.annotation.tailrec
import scala.io.Source

object Advent21 extends App:
  opaque type Allergen = String
  opaque type Ingredient = String

  def readRules(fileName: String): Rules =
    val RegEx = """(.+) \(contains (.+)\)""".r
    def readLine(line: String): Entry = line match
      case RegEx(ingredients, allergens) => Entry(
          ingredients.split(' ').toSet.map(x => Ingredient(x)), 
          allergens.split(", ").toSet.map(x => Allergen(x)),
        )
      case _ => sys.error(s"Couldn't parse $line")
        
    val entries = Source
      .fromResource(fileName)
      .getLines()
      .toSet
      .map(readLine)
    
    Rules(entries)
  
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
  
  def findCanonical(rules: Rules): Map[Allergen, Ingredient] =
    val simplified: Map[Allergen, Set[Ingredient]] = rules
      .allergens.map { allergen =>
        allergen -> rules.ingredientsWhichCanContain(allergen)
      }
      .toMap
    
    @tailrec
    def deduce(remaining: Map[Allergen, Set[Ingredient]], acc: Map[Allergen, Ingredient]): Map[Allergen, Ingredient] =
      if (remaining.isEmpty) then acc else
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

  def findSafe(rules: Rules, canonical: Map[Allergen, Ingredient]): Set[Ingredient] =
    val allergicIngredients = canonical.values.toSet
    rules.ingredients -- allergicIngredients

  def run(fileName: String, expected1: Int, expected2: String) =
    val rules = readRules(fileName)
    val canonical = findCanonical(rules)
    val safe = findSafe(rules, canonical)
    val solution1 = safe
      .toList
      .map { ingredient => 
        rules.entries.count(_.ingredients.contains(ingredient))
      }
      .sum
    
    println(solution1)
    assert(solution1 == expected1)
    
    val solution2 = canonical
      .toList
      .sortBy { case (allergen, ingredient) =>
        allergen
      }
      .map { case (allergen, ingredient) =>
        ingredient
      }
      .mkString(",")
  
    assert(solution2 == expected2)
    println(solution2)
  
  run("21-test.txt", 5, "mxmxvkd,sqjhc,fvjkl")
  run("21.txt", 2826, "pbhthx,sqdsxhb,dgvqv,csnfnl,dnlsjr,xzb,lkdg,rsvlb")  
  
  println("Passed")
