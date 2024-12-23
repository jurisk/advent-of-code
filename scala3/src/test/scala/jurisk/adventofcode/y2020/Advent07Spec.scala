package jurisk.adventofcode.y2020

import cats.implicits.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.util.Properties

class Advent07Spec extends AnyFreeSpec:
  private val tests1 = """
    |light red bags contain 1 bright white bag, 2 muted yellow bags.
    |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    |bright white bags contain 1 shiny gold bag.
    |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    |faded blue bags contain no other bags.
    |dotted black bags contain no other bags.""".stripMargin

  private def parse(x: String) = Advent07.parseInput(x.split(Properties.lineSeparator).filter(_.nonEmpty).iterator)

  private val testCases1 = parse(tests1)

  "solution1" in {
    val solved11 = testCases1 map Advent07.solution1
    solved11 shouldEqual 4.asRight
  }

  private val tests2 =
    """
      |shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

  private val testCases2 = parse(tests2)

  "solution2" - {
    "1" in {
      val solved21 = testCases1 map Advent07.solution2
      solved21 shouldEqual 32.asRight
    }

    "2" in {
      val solved22 = testCases2 map Advent07.solution2
      solved22 shouldEqual 126.asRight
    }
  }
