package jurisk.adventofcode.y2022

import jurisk.utils.FileInput.parseFileLines
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent03 {
  type Parsed = List[Rucksack]
  type Result = Int

  final case class Item(ch: Char, priority: Int)

  object Item {
    def parse(ch: Char): Item = {
      val value = ch match {
        case ch if ch >= 'a' && ch <= 'z' => ch - 'a' + 1
        case ch if ch >= 'A' && ch <= 'Z' => ch - 'A' + 27
        case _                            => sys.error(s"Unrecognized char: $ch")
      }

      Item(ch, value)
    }

    def singleSharedItem(groups: List[Set[Item]]): Item =
      groups.reduce(_ intersect _).singleElementUnsafe
  }

  final case class Rucksack(first: Set[Item], second: Set[Item]) {
    def itemInBothCompartments: Item =
      Item.singleSharedItem(first :: second :: Nil)

    def allItems: Set[Item] = first ++ second
  }

  object Rucksack {
    def parse(s: String): Rucksack = {
      require(
        s.length % 2 == 0,
        s"Rucksack $s does not divide into 2 equal compartments",
      )
      val (a, b) = s.map(Item.parse).splitAt(s.length / 2)
      Rucksack(a.toSet, b.toSet)
    }

    def singleItemInAll(rucksacks: List[Rucksack]): Item =
      Item.singleSharedItem(rucksacks.map(_.allItems))
  }

  def parse(fileName: String): Parsed =
    parseFileLines(fileName, Rucksack.parse)

  def part1(data: Parsed): Result =
    data.map(_.itemInBothCompartments).map(_.priority).sum

  def part2(data: Parsed): Result = {
    require(data.length % 3 == 0)
    val groups = data.grouped(3)
    groups.map(Rucksack.singleItemInAll).map(_.priority).sum
  }

  def main(args: Array[String]): Unit = {
    val test = parse("2022/03-test.txt")
    val real = parse("2022/03.txt")

    part1(test) shouldEqual 157
    part1(real) shouldEqual 7763

    part2(test) shouldEqual 70
    part2(real) shouldEqual 2569
  }
}
