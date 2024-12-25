package jurisk.adventofcode.y2024

import jurisk.algorithms.graph.GraphAlgorithms.createAdjacencyMapUndirected
import jurisk.algorithms.graph.GraphAlgorithms.enumerateMaximumCliques
import jurisk.collections.immutable.SetOfTwo
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent23 {
  type Input = Map[Computer, Set[Computer]]
  type N     = Int

  final case class Computer(a: Char, b: Char) {
    def startsWith(char: Char): Boolean = a == char
    override def toString: String       = s"$a$b"
  }

  object Computer {
    implicit val computerOrdering: Ordering[Computer] =
      Ordering.by[Computer, Char](_.a).orElseBy(_.b)

    def parse(s: String): Computer = {
      val (a, b) = s.toList.twoElementsUnsafe
      Computer(a, b)
    }
  }

  def parse(input: String): Input = {
    val connections = input
      .parseLines { s =>
        s.parsePairUnsafe("-", Computer.parse, Computer.parse)
      }
      .map { case (a, b) =>
        SetOfTwo(a, b)
      }

    createAdjacencyMapUndirected(connections)
  }

  def part1(connections: Input): N = {
    var results = Set.empty[Set[Computer]]

    enumerateMaximumCliques[Computer](
      connections,
      clique =>
        clique.toSeq.combinations(3) foreach { triplet =>
          println(triplet)
          if (triplet.exists(_.startsWith('t'))) {
            results += triplet.toSet
          }
        },
    )

    results.size
  }

  def part2(data: Input): String = {
    var best = Set.empty[Computer]

    enumerateMaximumCliques[Computer](
      data,
      clique =>
        if (clique.size > best.size) {
          best = clique
        },
    )

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
