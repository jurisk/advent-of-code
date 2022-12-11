package jurisk.adventofcode.y2022

import cats.implicits.catsSyntaxEitherId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import jurisk.utils.Utils.IterableOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

object Advent11 {
  type Parsed = List[Monkey]
  type Item   = BigInt
  type Index  = Int

  final case class Monkey(
    index: Index,
    items: List[Item],
    operation: Item => Item,
    testIsDivisibleBy: Int,
    ifTrueThrowTo: Index,
    ifFalseThrowTo: Index,
    inspectedItems: Long,
  ) {
    def increaseCounter: Monkey =
      copy(inspectedItems = inspectedItems + 1)

    def addItem(item: Item): Monkey =
      copy(items = items ::: List(item))

    override def toString: String =
      s"Monkey $index inspected items $inspectedItems times and has ${items.length} items: ${items.map(_.toString).mkString(", ")}"
  }

  object Monkey {
    def parse(s: String): Monkey = {
      val Array(m, si, o, t, iT, iF) = s.split("\n")

      Monkey(
        index = m.extractInts.singleElementUnsafe,
        items = si.extractInts.map(_.toLong),
        operation = o match {
          case "  Operation: new = old * old" =>
            old => old * old

          case x if x.startsWith("  Operation: new = old * ") =>
            old => old * o.extractInts.singleElementUnsafe

          case x if x.startsWith("  Operation: new = old + ") =>
            old => old + o.extractInts.singleElementUnsafe

          case _ => sys.error(s"Did not recognize $o")
        },
        testIsDivisibleBy = t.extractInts.singleElementUnsafe,
        ifTrueThrowTo = iT.extractInts.singleElementUnsafe,
        ifFalseThrowTo = iF.extractInts.singleElementUnsafe,
        inspectedItems = 0L,
      )
    }
  }

  def parse(data: String): Parsed =
    data.parseList("\n\n", Monkey.parse)

  private def simplify1(n: BigInt): BigInt = {
    val dividedLevel = (BigDecimal(n) / BigDecimal
      .decimal(3)).setScale(0, RoundingMode.FLOOR).toBigIntExact.get
    if (debugPrint)
      println(
        s"      Monkey gets bored with item. Worry level is divided by 3 to $dividedLevel"
      )
    dividedLevel
  }

  private def simplify2(n: BigInt): BigInt =
    // val failedAttempt = dividedLevel % (11 * 2 * 5 * 17 * 19 * 7 * 3 * 13)
    // TODO
    n

  private val debugPrint = false
  private def convert(
    monkey: Monkey,
    item: Item,
    simplify: BigInt => BigInt,
  ): (Index, Item) = {
    if (debugPrint)
      println(s"  Monkey inspects an item with a worry level of $item.")
    val newLevel     = monkey.operation(item)
    if (debugPrint) println(s"      Worry level is ... to $newLevel")
    val dividedLevel = simplify(newLevel)
    val testResult   = dividedLevel % monkey.testIsDivisibleBy == 0
    val throwTo      = if (testResult) {
      if (debugPrint)
        println(
          s"     Current worry level is divisible by ${monkey.testIsDivisibleBy}."
        )
      monkey.ifTrueThrowTo
    } else {
      if (debugPrint)
        println(
          s"      Current worry level is not divisible by ${monkey.testIsDivisibleBy}."
        )
      monkey.ifFalseThrowTo
    }

    if (debugPrint)
      println(
        s"      Item with worry level $dividedLevel is thrown to monkey $throwTo."
      )
    (throwTo, dividedLevel)
  }

  private def processItem(
    fromMonkey: Index,
    item: Item,
    monkeys: List[Monkey],
    simplify: BigInt => BigInt,
  ): List[Monkey] = {
    val monkey              = monkeys(fromMonkey)
    val (throwTo, newValue) = convert(monkey, item, simplify)
    monkeys
      .updated(fromMonkey, monkey.increaseCounter)
      .updated(throwTo, monkeys(throwTo).addItem(newValue))
  }

  @tailrec
  private def processMonkey(
    index: Index,
    monkeys: List[Monkey],
    simplify: BigInt => BigInt,
  ): List[Monkey] = {
    if (debugPrint) println(s"Monkey $index:")
    val thisMonkey = monkeys(index)
    thisMonkey.items match {
      case h :: t =>
        val resulting = processItem(
          index,
          h,
          monkeys.updated(index, thisMonkey.copy(items = t)),
          simplify,
        )
        processMonkey(index, resulting, simplify)

      case Nil =>
        if (debugPrint) println("     Out of items\n")
        monkeys // no more items to process
    }
  }

  private def processAllMonkeys(
    monkeys: List[Monkey],
    simplify: BigInt => BigInt,
  ): List[Monkey] = {
    var current = monkeys
    monkeys.indices foreach { monkeyIdx =>
      val result = processMonkey(monkeyIdx, current, simplify)
      current = result
    }
    current
  }

  private def simulate(
    initialMonkeys: Parsed,
    rounds: Int,
    simplify: BigInt => BigInt,
  ): Long = {
    println(initialMonkeys.map(_.toString).mkString("\n"))
    println

    Simulation.runWithIterationCount(initialMonkeys) {
      case (monkeys, iteration) =>
        if (iteration % 20 == 0) {
          println(s"Iteration $iteration:")
          monkeys.zipWithIndex foreach { case (monkey, index) =>
            println(
              s"Monkey $index inspected items ${monkey.inspectedItems} times"
            )
          }
          println
        }

        if (iteration >= rounds) {
          val result = monkeys
            .map(_.inspectedItems)
            .sorted(Ordering[Long].reverse)
            .take(2)
            .product
          println(s"Got $result")
          result.asLeft
        } else {
          val newMonkeys = processAllMonkeys(monkeys, simplify)
          newMonkeys.asRight
        }
    }
  }

  object RandomExploration {
    def f0(n: Int): (Index, Int) =
      if (n % 11 == 0) {
        f3(n * 5)
      } else {
        f4(n * 5)
      }

    def f1(n: Int): (Index, Int) =
      if (n % 2 == 0) {
        f6(n * n)
      } else {
        f7(n * n)
      }

    def f2(n: Int): (Index, Int) =
      if (n % 5 == 0) {
        (1, n * 7)
      } else {
        f5(n * 7)
      }

    def f3(n: Int): (Index, Int) =
      if (n % 17 == 0) {
        (2, n + 1)
      } else {
        f5(n + 1)
      }

    def f4(n: Int): (Index, Int) =
      if (n % 19 == 0) {
        (2, n + 3)
      } else {
        (3, n + 3)
      }

    def f5(n: Int): (Index, Int) =
      if (n % 7 == 0) {
        (1, n + 5)
      } else {
        f6(n + 5)
      }

    def f6(n: Int): (Index, Int) =
      if (n % 3 == 0) {
        (0, n + 8)
      } else {
        // f7(n + 8)
        if ((n + 8) % 13 == 0) {
          (4, n + 10)
        } else {
          (0, n + 10)
        }
      }

    def f7(n: Int): (Index, Int) =
      if (n % 13 == 0) {
        (4, n + 2)
      } else {
        (0, n + 2)
      }
  }

  private def part1(parsed: Advent11.Parsed): Long =
    simulate(parsed, 20, simplify1)

  private def part2(parsed: Advent11.Parsed): Long =
    simulate(parsed, 10000, simplify2)

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/11-test.txt")
    val realData = readFileText("2022/11.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 10605
    part1(real) shouldEqual 120756

    part2(test) shouldEqual 2713310158L
    part2(real) shouldEqual Long.MaxValue
  }
}
