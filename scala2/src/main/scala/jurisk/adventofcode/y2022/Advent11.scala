package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import jurisk.utils.CollectionOps.IterableOps
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
      index = m.extractIntList.singleElementUnsafe,
      operation = o match {
        case "  Operation: new = old * old" =>
          old => old * old

        case s"  Operation: new = old * $n" =>
          old => old * n.toInt

        case s"  Operation: new = old + $n" =>
          old => old + n.toInt

        case _ => sys.error(s"Did not recognize $o")
      },
      testIsDivisibleBy = t.extractIntList.singleElementUnsafe,
      ifTrueThrowTo = iT.extractIntList.singleElementUnsafe,
      ifFalseThrowTo = iF.extractIntList.singleElementUnsafe,
    )

    val itemStates = si.extractIntList.map(_.toLong) map { x =>
      ItemState(
        logic.index,
        x,
      )
    }

    (logic, itemStates)
  }

  def parse(data: String): Parsed = {
    val list   = data.parseList("\n\n", parseMonkey)
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

  private def createSimplify2(logic: List[MonkeyLogic]): BigInt => BigInt = {
    val moduloWith = logic.map(_.testIsDivisibleBy).product
    (n: BigInt) => n % moduloWith
  }

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

  private def addCounters(a: Counters, b: Counters): Counters =
    (a zip b).map { case (x, y) => x + y }

  private def printCounters(counters: Counters): Unit =
    counters.zipWithIndex foreach { case (value, index) =>
      println(s"Monkey $index inspected items $value times")
    }

  private def simulate(
    monkeyLogic: List[MonkeyLogic],
    itemStates: List[ItemState],
    rounds: Int,
    simplify: BigInt => BigInt,
  ): Long = {
    @tailrec
    def process(
      itemState: ItemState,
      simplify: Item => Item,
      counters: Counters = List.fill(monkeyLogic.length)(0),
    ): (ItemState, Counters) = {
      val logic                  = monkeyLogic(itemState.atMonkey)
      val (nextIndex, nextValue) = convert(logic, itemState.value, simplify)
      val newAcc                 =
        counters.updated(itemState.atMonkey, counters(itemState.atMonkey) + 1)
      val newState               = ItemState(nextIndex, nextValue)
      if (nextIndex <= itemState.atMonkey) {
        (newState, newAcc)
      } else {
        process(newState, simplify, newAcc)
      }
    }

    Simulation.runWithIterationCount(
      (itemStates, List.fill(monkeyLogic.length)(0L))
    ) { case (acc, iteration) =>
      val (itemStates, monkeyCounters) = acc
      if (iteration % 20 == 0) {
        println(s"Iteration $iteration:")
        monkeyCounters.zipWithIndex foreach { case (value, index) =>
          println(s"Monkey $index inspected items $value times")
        }
        println()
      }

      if (iteration >= rounds) {
        val result = monkeyBusiness(monkeyCounters)
        println(s"Got $result")
        result.asLeft
      } else {
        val results = itemStates map { itemState =>
          process(itemState, simplify)
        }

        val (newItemStates, counterDeltas) = results.unzip

        val newCounterDeltas: Counters =
          counterDeltas.foldLeft[Counters](monkeyCounters) { case (a, b) =>
            addCounters(a, b)
          }

        (newItemStates, newCounterDeltas).asRight
      }
    }
  }

  private def part1(parsed: Advent11.Parsed, rounds: Int): Long = {
    val (logic, state) = parsed
    simulate(logic, state, rounds, simplify1)
  }

  private def part2(parsed: Advent11.Parsed, rounds: Int): Long = {
    val (logic, state) = parsed
    simulate(logic, state, rounds, createSimplify2(logic))
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/11-test.txt")
    val realData = readFileText("2022/11.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test, 20) shouldEqual 101 * 105
    part1(real, 20) shouldEqual 120756

    part2(test, 1) shouldEqual 4 * 6
    part2(test, 20) shouldEqual 99 * 103

    part2(test, 10000) shouldEqual 52166L * 52013L
    part2(real, 10000) shouldEqual 39109444654L
  }
}
