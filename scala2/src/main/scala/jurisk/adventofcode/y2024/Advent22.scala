package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object Advent22 {
  type Input = List[N]
  type N     = Long
  private val SequenceLength = 4

  def parse(input: String): Input =
    input.parseLines(_.toLong)

  def next(n: N): N = {
    def mixPrune(a: N, b: N): N =
      (a ^ b) % 16777216

    val a = mixPrune(n * 64, n)
    val b = mixPrune(a / 32, a)
    mixPrune(b * 2048, b)
  }

  @tailrec
  def nthSecretNumber(n: N, nth: Int): N =
    if (nth == 0) {
      n
    } else {
      nthSecretNumber(next(n), nth - 1)
    }

  def bananasAndDiffs(
    secret: N,
    howMany: Int = 2000,
  ): (IndexedSeq[N], IndexedSeq[N]) = {
    val bananas = ArrayBuffer.fill(howMany + 1)(Long.MaxValue)
    bananas(0) = secret
    for (i <- 0 until howMany)
      bananas(i + 1) = next(bananas(i))

    val diffs = ArrayBuffer.fill(howMany)(Long.MaxValue)
    for (i <- 0 until howMany)
      diffs(i) = bananas(i + 1) % 10 - bananas(i) % 10

    (bananas.map(_ % 10).toIndexedSeq, diffs.toIndexedSeq)
  }

  def bananasFromSequence(
    bananasAndDiffs: (IndexedSeq[N], IndexedSeq[N]),
    sequence: IndexedSeq[N],
  ): Option[N] = {
    assert(sequence.length == SequenceLength)
    val (bananas, diffs) = bananasAndDiffs
    diffs.sliding(SequenceLength).zipWithIndex.foreach { case (diffs, i) =>
      if (diffs == sequence) {
        return bananas(i + SequenceLength).some
      }
    }
    None
  }

  def createBananaMap(secret: N): Map[List[N], N] = {
    var map: Map[List[N], N] = Map.empty
    val (bananas, diffs)     = bananasAndDiffs(secret)
    diffs.sliding(SequenceLength).zipWithIndex.foreach { case (diffs4, i) =>
      val result    = bananas(i + SequenceLength)
      val diffsList = diffs4.toList
      map = map.updatedWith(diffsList)(_.getOrElse(result).some)
    }
    map
  }

  def part1(data: Input): N =
    data.map(n => nthSecretNumber(n, 2000)).sum

  def part2(data: Input): N = {
    val maps                           = data.map(createBananaMap)
    val keys                           = maps.flatMap(_.keys).toSet
    def bananasForKey(key: List[N]): N = maps.flatMap(_.get(key)).sum
    keys.map(bananasForKey).max
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/22$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
