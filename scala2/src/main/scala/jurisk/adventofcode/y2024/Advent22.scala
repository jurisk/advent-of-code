package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import jurisk.utils.CollectionOps.IteratorOps
import jurisk.utils.CollectionUtils.mergeMaps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
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
    diffs
      .sliding(SequenceLength)
      .firstIndexOf(sequence)
      .map(i => bananas(i + SequenceLength))
  }

  def createBananaMap(secret: N): Map[IndexedSeq[N], N] = {
    val (bananas, diffs) = bananasAndDiffs(secret)

    diffs
      .sliding(SequenceLength)
      .zipWithIndex
      .foldLeft(Map.empty[IndexedSeq[N], N]) { case (acc, (diffs, i)) =>
        val result = bananas(i + SequenceLength)
        acc.updatedWith(diffs)(_.getOrElse(result).some)
      }
  }

  def part1(data: Input): N =
    data.map(n => nthSecretNumber(n, 2000)).sum

  def part2(data: Input): N =
    mergeMaps(data.map(createBananaMap)).values.map(_.sum).max

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
