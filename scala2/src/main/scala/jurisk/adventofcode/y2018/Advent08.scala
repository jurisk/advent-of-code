package jurisk.adventofcode.y2018

import jurisk.utils.FileInput.readSingleFileLine
import jurisk.utils.Parsing.StringOps
import jurisk.utils.CollectionOps.IterableOps
import org.scalatest.matchers.should.Matchers._

object Advent08 {
  final case class Node(
    children: List[Node],
    metadata: List[Int],
  ) {
    def recursiveSum: Int = metadata.sum + children.map(_.recursiveSum).sum

    def rootNode: Int =
      if (children.isEmpty)
        recursiveSum
      else
        metadata.map { idx =>
          children.lift(idx - 1).map(_.rootNode).getOrElse(0)
        }.sum
  }

  def readFileAndParse(fileName: String): List[Int] =
    readSingleFileLine(fileName).parseList(" ", _.toInt)

  private def parseNodes(n: Int, data: List[Int]): (List[Node], List[Int]) =
    if (n == 0) {
      (Nil, data)
    } else {
      data match {
        case childN :: metadataN :: tail =>
          val (children, remaining) = parseNodes(childN, tail)
          val (metadata, rem)       = remaining.splitAt(metadataN)
          val node                  = Node(children, metadata)
          val (otherNodes, remm)    = parseNodes(n - 1, rem)
          val nodes                 = node :: otherNodes

          (nodes, remm)

        case _ => data.toString.failedToParse
      }
    }

  def process(data: List[Int]): Node = {
    val (result, remaining) = parseNodes(1, data)
    require(remaining.isEmpty)
    result.singleElementUnsafe
  }

  def part1(data: List[Int]): Int =
    process(data).recursiveSum

  def part2(data: List[Int]): Int =
    process(data).rootNode

  def main(args: Array[String]): Unit = {
    val test = readFileAndParse("2018/08-test.txt")
    val real = readFileAndParse("2018/08.txt")

    part1(test) shouldEqual 138
    part1(real) shouldEqual 45868

    part2(test) shouldEqual 66
    part2(real) shouldEqual 19724
  }
}
