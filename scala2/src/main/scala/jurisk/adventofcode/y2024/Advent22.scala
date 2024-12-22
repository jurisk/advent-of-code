package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.mutable.ArrayBuffer

object Advent22 {
  type Input = List[N]
  type N     = Long

  def parse(input: String): Input =
    input.parseLines(_.toLong)

  def mixPrune(a: N, b: N): N =
    (a ^ b) % 16777216

  def next(n: N): N = {
    val a = mixPrune(n * 64, n)
    val b = mixPrune(a / 32, a)
    val c = mixPrune(b * 2048, b)
    c
  }

  def nthSecretNumber(n: N, nth: Int): N = {
    var current = n
    for (_ <- 0 until nth)
      current = next(current)
    current
  }

  def bananasAndDiffs(secret: N, howMany: Int = 2000): (Array[N], Array[N]) = {
    val bananas = ArrayBuffer.fill(howMany + 1)(Long.MaxValue)
    bananas(0) = secret
    for (i <- 0 until howMany)
      bananas(i + 1) = next(bananas(i))

    val diffs = ArrayBuffer.fill(howMany)(Long.MaxValue)
    for (i <- 0 until howMany)
      diffs(i) = bananas(i + 1) % 10 - bananas(i) % 10

    (bananas.map(_ % 10).toArray, diffs.toArray)
  }

  def bananasFromSequence(
    bananasAndDiffs: (Array[N], Array[N]),
    sequence: Array[N],
  ): Option[N] = {
    assert(sequence.length == 4)
    val (bananas, diffs) = bananasAndDiffs
    diffs.sliding(4).zipWithIndex.foreach { case (diffs4, i) =>
      if (diffs4.sameElements(sequence)) {
        return Some(bananas(i + 4))
      }
    }
    None
  }

  def createBananaMap(secret: N): Map[List[N], N] = {
    var map: Map[List[N], N] = Map.empty
    val (bananas, diffs)     = bananasAndDiffs(secret)
    diffs.sliding(4).zipWithIndex.foreach { case (diffs4, i) =>
      val result    = bananas(i + 4)
      val diffsList = diffs4.toList
      if (!map.contains(diffsList)) {
        map = map + (diffsList -> result)
      }
    }
    map
  }

  def part1(data: Input): N =
    data.map(n => nthSecretNumber(n, 2000)).sum

  def part2(data: Input): N = {
    val maps = data.map(createBananaMap)

    val keys = maps.flatMap(_.keys).toSet

    def bananasForKey(key: List[N]): N =
      maps.flatMap { map =>
        map.get(key)
      }.sum

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
