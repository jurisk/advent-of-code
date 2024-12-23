package jurisk.adventofcode.y2024

import jurisk.algorithms.pathfinding.ConnectedComponents
import jurisk.collections.immutable.SetOfTwo
import jurisk.collections.mutable.DisjointSets
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec
import scala.collection.mutable

object Advent23 {
  type Input      = List[Connection]
  type Computer   = String
  type Connection = SetOfTwo[Computer]
  type N          = Long

  def parse(input: String): Input =
    input.parseLines { s =>
      val (a, b) = s.splitPairUnsafe("-")
      SetOfTwo(a, b)
    }

  def part1(data: Input): N = {
    val computers = data.flatMap(_.toSet).toSet
    println(computers)

    val connections = computers.map { c =>
      c -> data.filter(_.contains(c)).flatMap(_.toSet).toSet
    }.toMap

    val withT =
      computers.toList.combinations(3).filter(_.exists(_.startsWith("t")))

    withT.filter {
      case List(a, b, c) =>
        connections(a).contains(b) && connections(b).contains(c) && connections(
          c
        ).contains(a)
      case _             => "fail".fail

    }.size
  }

  def part2(data: Input): String = {
    val computers = data.flatMap(_.toSet).toSet

    val connections = computers.map { c =>
      c -> data.filter(_.contains(c)).flatMap(_.toSet).toSet
    }.toMap

    val disjointSets = DisjointSets(computers.toList: _*)
    data foreach { d =>
      val (a, b) = d.tupleInArbitraryOrder
      disjointSets.union(a, b)
    }

    @tailrec
    def f(current: Set[Computer], remaining: Set[Computer]): Set[Computer] =
      remaining.find { candidate =>
        current.forall(connections(candidate).contains)
      } match {
        case Some(next) =>
          f(current + next, remaining - next)
        case None       =>
          current
      }

    def findSingleClique(remaining: Set[Computer]): Set[Computer] = {
      val first = remaining.head
      val rem   = remaining - first
      f(Set(first), rem)
    }

    var best      = Set.empty[Computer]
    var toProcess = computers
    while (toProcess != Set.empty) {
      var singleClique = findSingleClique(toProcess)
      println(singleClique)
      if (singleClique.size > best.size) {
        best = singleClique
      }
      toProcess --= singleClique
    }

    best.toList.sorted.mkString(",")
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/23$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
