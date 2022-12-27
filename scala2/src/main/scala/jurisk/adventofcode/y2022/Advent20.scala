package jurisk.adventofcode.y2022

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent20 {
  type Index = Int

  def parse(data: String): Vector[Long] =
    data.parseList("\n", _.toLong).toVector

  private def moveNumber(
    vector: Vector[Index],
    atIndex: Int,
    howFar: Long,
  ): Vector[Index] = {
    val a               = vector.take(atIndex)
    val value           = vector(atIndex)
    val b               = vector.drop(atIndex + 1)
    val rejoined        = a ++ b
    val newPosition     = (atIndex + howFar) % rejoined.length
    val clampedPosition = (if (newPosition >= 0) newPosition % rejoined.length
                           else newPosition + rejoined.length).toInt
    require(clampedPosition >= 0)
    require(clampedPosition < rejoined.length)
    rejoined.take(clampedPosition) ++ Vector(value) ++ rejoined.drop(
      clampedPosition
    )
  }

  private def mix(lookup: Vector[Long], timesToMix: Int): Vector[Long] = {
    val indices = lookup.indices.toVector
    val vector  = Vector.fill(timesToMix)(indices).flatten

    val result = vector.foldLeft(indices) { case (acc, idx) =>
      val index = acc.indexOf(idx)
      require(index != -1)
      moveNumber(acc, index, lookup(idx))
    }

    result map lookup
  }

  def solve(
    lookup: Vector[Long],
    decryptionKey: Long,
    timesToMix: Int,
  ): Long = {
    val mixed: Vector[Long] = mix(lookup.map(_ * decryptionKey), timesToMix)
    require(mixed.length == lookup.length)
    val indexOf0            = mixed.indexOf(0L)
    require(indexOf0 != -1)
    List(1000, 2000, 3000).map { x =>
      mixed((indexOf0 + x) % mixed.length)
    }.sum
  }

  def part1(lookup: Vector[Long]): Long =
    solve(lookup, 1, 1)

  def part2(lookup: Vector[Long]): Long = {
    val DecryptionKey = 811589153L
    val TimesToMix    = 10

    solve(lookup, DecryptionKey, TimesToMix)
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2022/20-test.txt")
    val realData = readFileText("2022/20.txt")

    val test = parse(testData)
    val real = parse(realData)

    part1(test) shouldEqual 3
    part1(real) shouldEqual 13183

    part2(test) shouldEqual 1623178306L
    part2(real) shouldEqual 6676132372578L
  }
}
