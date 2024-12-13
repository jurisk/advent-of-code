package jurisk.adventofcode.y2024

import cats.implicits.catsSyntaxOptionId
import jurisk.geometry.Coordinates2D
import jurisk.optimization.ImplicitConversions.{
  RichArithExprIntSort,
  RichExpr,
  RichLong,
}
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.annotation.nowarn
import scala.util.Try

object Advent13 {
  type Input = List[Machine]
  type N     = Long
  type C     = Coordinates2D[Long]

  final case class Machine(
    buttonA: C,
    buttonB: C,
    prize: C,
  ) {
    def solve: Option[N] = {
      val results = for {
        a     <- 0L to 100
        b     <- 0L to 100
        result = buttonA * a + buttonB * b
        if result == prize
      } yield 3 * a + b

      val result = results.minOption
      println(s"$this $results $result")
      result
    }

    def solve2: Option[N] = Try {
      solve2Q
    }.toOption.flatten

    def solve2Q: Option[N] = {
      println(s"Trying to solve $this")
      // a * ax + b * bx = px
      // a * ay + b * by = py

      implicit val optimizer: Optimizer = Optimizer.z3()
      import optimizer._

      val List(a, b, cost) = List("a", "b", "cost").map(labeledInt)
      val (ax, bx, px)     =
        (buttonA.x.constant, buttonB.x.constant, prize.x.constant)
      val (ay, by, py)     =
        (buttonA.y.constant, buttonB.y.constant, prize.y.constant)

      addConstraints(
        a >= Zero,
        b >= Zero,
        a * ax + b * bx === px,
        a * ay + b * by === py,
        cost === 3.constant * a + b,
      )

      val _ = minimize(cost)

      @nowarn("cat=deprecation")
      val m = checkAndGetModel()
      debugPrint()

      val ar = m.getConstInterp(a).toString
      val br = m.getConstInterp(b).toString

      (ar.toLong * 3 + br.toLong).some

//      println(m.toString)
//      m.evaluate(cost, true).getString.toLong.some
//      val List(ar, br) = runExternal("a", "b").map(resultToLong)
//      val result = 3 * ar + br
//      result.some
    }
  }

  object Machine {
    def parse(s: String): Machine = {
      def parseCoords(s: String, symbol: String): C = {
        val Pattern = s"""X$symbol(\\d+), Y$symbol(\\d+)""".r
        s match {
          case Pattern(x, y) => Coordinates2D(x.toLong, y.toLong)
          case _             => s.fail
        }
      }

      val List(as, bs, ps) = s.split("\n").toList
      val a                = as.removePrefixUnsafe("Button A: ")
      val b                = bs.removePrefixUnsafe("Button B: ")
      val p                = ps.removePrefixUnsafe("Prize: ")
      Machine(parseCoords(a, "\\+"), parseCoords(b, "\\+"), parseCoords(p, "="))
    }
  }

  def parse(input: String): Input =
    input.parseSections(Machine.parse)

  def part1(data: Input): N =
    data.flatMap(_.solve).sum

  def part2(data: Input): N = {
    val adjusted = data.map(m =>
      m.copy(prize = m.prize + Coordinates2D(10000000000000L, 10000000000000L))
    )

    adjusted.flatMap(_.solve2).sum
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2024/13$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
