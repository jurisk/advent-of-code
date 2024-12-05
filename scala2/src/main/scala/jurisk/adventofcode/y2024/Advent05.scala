package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxPartialOrder
import jurisk.utils.CollectionOps.SeqOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent05 {
  private type Page = Int

  final case class PrintedBefore(
    page: Page,
    mustBePrintedBeforePage: Page,
  ) {
    def matches(pages: List[Page]): Boolean =
      (
        pages firstIndexOf page,
        pages firstIndexOf mustBePrintedBeforePage,
      ) match {
        case (Some(pageIdx), Some(mustBePrintedBeforePageIdx)) =>
          pageIdx < mustBePrintedBeforePageIdx
        case _                                                 => true
      }
  }

  private object PrintedBefore {
    def parse(s: String): PrintedBefore = {
      val (page, mustBePrintedBeforePage) =
        s.parsePairUnsafe('|', _.toInt, _.toInt)
      PrintedBefore(page, mustBePrintedBeforePage)
    }
  }

  final case class Update(pages: List[Page]) {
    def isValid(pageOrderingRules: List[PrintedBefore]): Boolean =
      pageOrderingRules.forall(_.matches(pages))

    def middlePage: Page =
      pages(pages.length / 2)

    def fixInvalid(pageOrderingRules: List[PrintedBefore]): Update = Update {
      pages.sortWith { case (a, b) =>
        if (
          pageOrderingRules
            .exists(r => r.page == a && r.mustBePrintedBeforePage == b)
        ) {
          true
        } else if (
          pageOrderingRules
            .exists(r => r.page == b && r.mustBePrintedBeforePage == a)
        ) {
          false
        } else {
          val aIdx = pages firstIndexOf a
          val bIdx = pages firstIndexOf b
          aIdx < bIdx
        }
      }
    }
  }

  private object Update {
    def parse(s: String): Update = Update {
      s.parseCommaSeparatedList(_.toInt)
    }
  }

  final case class Input(
    pageOrderingRules: List[PrintedBefore],
    updates: List[Update],
  )

  def parse(input: String): Input = {
    val (a, b)            = input.splitPairByDoubleNewline
    val updates           = b parseLines Update.parse
    val pageOrderingRules = a parseLines PrintedBefore.parse
    Input(pageOrderingRules, updates)
  }

  def part1(data: Input): Int =
    data.updates.filter(_.isValid(data.pageOrderingRules)).map(_.middlePage).sum

  def part2(data: Input): Int = {
    val invalids = data.updates.filterNot(_.isValid(data.pageOrderingRules))
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
