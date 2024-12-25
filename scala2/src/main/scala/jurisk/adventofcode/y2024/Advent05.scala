package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxPartialOrder
import jurisk.utils.CollectionOps.SeqOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.tailrec

// TODO: This should use topological sort and you have it in `GraphAlgorithms`
object Advent05 {
  private type Page = Int

  final case class Update(pages: List[Page], index: Map[Page, Int]) {
    def middlePage: Page =
      pages(pages.length / 2)

    def fixInvalid(pageOrderingRules: PageOrderingRules): Update = Update {
      pages.sortWith { case (a, b) =>
        if (pageOrderingRules.contains(a, b)) {
          true
        } else if (pageOrderingRules.contains(b, a)) {
          false
        } else {
          index.get(a) < index.get(b)
        }
      }
    }
  }

  private object Update {
    def apply(pages: List[Page]): Update = {
      val index = pages.zipWithIndex.toMap
      new Update(pages, index)
    }

    def parse(s: String): Update = Update {
      s.parseCommaSeparatedList(_.toInt)
    }
  }

  final case class PageOrderingRules(preconditions: Map[Page, Set[Page]]) {
    def validFor(update: Update): Boolean = {
      @tailrec
      def f(pages: List[Page], remaining: Set[Page]): Boolean =
        pages match {
          case Nil    => true
          case h :: t =>
            val pre = preconditions.getOrElse(h, Set.empty)
            if ((pre intersect remaining).isEmpty) {
              f(t, remaining - h)
            } else {
              false
            }
        }

      f(update.pages, update.pages.toSet)
    }

    def contains(page: Page, precondition: Page): Boolean =
      preconditions.getOrElse(page, Set.empty) contains precondition
  }

  private object PageOrderingRules {
    def parse(input: String): PageOrderingRules = PageOrderingRules {
      input
        .parseLines { s =>
          val (page, mustBePrintedBeforePage) =
            s.parsePairUnsafe('|', _.toInt, _.toInt)
          (page, mustBePrintedBeforePage)
        }
        .groupBy { case (_, mustBePrintedBefore) => mustBePrintedBefore }
        .map { case (k, v) =>
          (k, v.map { case (page, _) => page }.toSet)
        }
    }
  }

  final case class Input(
    pageOrderingRules: PageOrderingRules,
    updates: List[Update],
  )

  def parse(input: String): Input = {
    val (a, b)            = input.splitPairByDoubleNewline
    val pageOrderingRules = PageOrderingRules.parse(a)
    val updates           = b parseLines Update.parse
    Input(pageOrderingRules, updates)
  }

  def part1(data: Input): Int =
    data.updates.filter(data.pageOrderingRules.validFor).map(_.middlePage).sum

  def part2(data: Input): Int = {
    val invalids = data.updates.filterNot(data.pageOrderingRules.validFor)
    val fixed    = invalids.map(_.fixInvalid(data.pageOrderingRules))
    fixed.map(_.middlePage).sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/05$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
