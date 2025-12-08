package jurisk.adventofcode.y2025

import jurisk.collections.immutable.SetOfTwo
import jurisk.collections.mutable.DisjointSets
import jurisk.geometry.Coords3D
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

object Advent08 {
  type C               = Int
  private type Point3D = Coords3D[C]
  type Input           = ArraySeq[Point3D]
  type Distance        = Long
  type N               = Long

  final class MutableState(
    val circuits: DisjointSets[Point3D],
    val sortedDistances: ArraySeq[(Distance, SetOfTwo[Point3D])],
    var nextToConnectIndex: Int = 0,
  ) {
    def connectMutable(): Unit = {
      val next        = sortedDistances(nextToConnectIndex)
      nextToConnectIndex += 1
      val (_, points) = next
      val (a, b)      = points.tupleInArbitraryOrder
      circuits.union(a, b)
    }

    override def toString: String = {
      val setSizes = circuits.toSets.toList.map(_.size).sorted
      s"MutableState(sizes=$setSizes, circuits=${circuits.toSets}"
    }
  }

  private def pointDistance(a: Point3D, b: Point3D): Distance = {
    val dx = (a.x - b.x).toLong
    val dy = (a.y - b.y).toLong
    val dz = (a.z - b.z).toLong
    dx * dx + dy * dy + dz * dz
  }

  private object MutableState {
    def make(points: ArraySeq[Point3D]): MutableState = {
      val distancesRemainingSorted = ArraySeq
        .from(
          points
            .combinations(2)
            .map {
              case Seq(a, b) =>
                val distance = pointDistance(a, b)
                (distance, SetOfTwo(a, b))
              case other     =>
                sys.error(s"Unexpected $other")
            }
            .toSet[(Distance, SetOfTwo[Point3D])]
        )
        .sortBy { case (distance, _) => distance }

      val circuits = DisjointSets[Point3D](points: _*)

      new MutableState(
        circuits,
        distancesRemainingSorted,
      )
    }
  }

  def parse(input: String): Input =
    ArraySeq.from(input.parseLines(Coords3D.parse[C]))

  def part1(
    data: Input,
    connectionsToMake: Int,
    largestCircuitsToMultiply: Int,
  ): N = {
    val state = MutableState.make(data)
    (0 until connectionsToMake).foreach { _ =>
      state.connectMutable()
    }

    state.circuits.toSets.toList
      .sortBy(-_.size)
      .take(largestCircuitsToMultiply)
      .map(_.size.toLong)
      .product
  }

  def part2(data: Input): N = {
    val state = MutableState.make(data)
    0
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/08$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 1000, 3)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
