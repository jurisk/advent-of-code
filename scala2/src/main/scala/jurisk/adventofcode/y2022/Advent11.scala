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
  type Parsed = (List[MonkeyLogic], List[ItemState])
  type Item   = BigInt
  type Index  = Int

  type Counters = List[Long]

  final case class ItemState(
    atMonkey: Index,
    value: Item,
  )

  final case class MonkeyLogic(
    index: Index,
    operation: Item => Item,
    testIsDivisibleBy: Int,
    ifTrueThrowTo: Index,
    ifFalseThrowTo: Index,
  )

  private def parseMonkey(s: String): (MonkeyLogic, List[ItemState]) = {
    val Array(m, si, o, t, iT, iF) = s.split("\n")

    val logic = MonkeyLogic(
      index = m.extractInts.singleElementUnsafe,
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
    )

    val itemStates = si.extractInts.map(_.toLong) map { x =>
      ItemState(
        logic.index,
        x,
      )
    }

    (logic, itemStates)
  }

  def parse(data: String): Parsed = {
    val list = data.parseList("\n\n", parseMonkey)
    val (a, b) = list.unzip
    (a, b.flatten)
  }

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
    monkey: MonkeyLogic,
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

  private def monkeyBusiness(data: List[Long]): Long = data
    .sorted(Ordering[Long].reverse)
    .take(2)
    .product

  private def addCounters(a: Counters, b: Counters): Counters = {
    (a zip b).map { case (x, y) => x + y }
  }

  private def combineCounters(counters: List[Counters]): Counters = {
    counters.reduce[Counters] { case (a, b) =>
      addCounters(a, b)
    }
  }

  private def printCounters(counters: Counters): Unit = {
    counters.zipWithIndex foreach { case (value, index) =>
      println(s"Monkey $index inspected items $value times")
    }
  }

  private def simulate(
    initial: Parsed,
    rounds: Int,
    simplify: BigInt => BigInt,
  ): Long = {
    val (monkeyLogic, itemStates) = initial

    @tailrec
    def process(itemState: ItemState, simplify: Item => Item, counters: Counters = List.fill(monkeyLogic.length)(0)): (ItemState, Counters) = {
      val logic = monkeyLogic(itemState.atMonkey)
      val (nextIndex, nextValue) = convert(logic, itemState.value, simplify)
      val newAcc = counters.updated(itemState.atMonkey, counters(itemState.atMonkey) + 1)
      val newState = ItemState(nextIndex, nextValue)
      if (nextIndex <= itemState.atMonkey) {
        (newState, newAcc)
      } else {
        process(newState, simplify, newAcc)
      }
    }

    val results = itemStates map { initialItemState =>
      Simulation.runWithIterationCount((initialItemState, List.fill(monkeyLogic.length)(0L))) {
        case (acc, iteration) =>
          val (itemState, monkeyCounters) = acc

          if (iteration >= rounds) {
            monkeyCounters.asLeft
          } else {
            val (newState, theseCounters) = process(itemState, simplify)
            val newCounters = addCounters(monkeyCounters, theseCounters)
            (newState, newCounters).asRight
          }
      }
    }

    val summed = combineCounters(results)

    println(s"Got $summed")
    val res = monkeyBusiness(summed)
    println(res)
    res
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
