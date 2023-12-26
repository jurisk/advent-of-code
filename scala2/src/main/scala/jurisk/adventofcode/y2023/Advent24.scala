package jurisk.adventofcode.y2023

import cats.implicits._
import com.microsoft.z3.Version
import jurisk.geometry.Area2D
import jurisk.geometry.Coordinates2D
import jurisk.geometry.Coords3D
import jurisk.geometry.Coords3D.Axis
import jurisk.math.positiveAndNegativeDivisors
import jurisk.optimization.ImplicitConversions.RichArithExprIntSort
import jurisk.optimization.ImplicitConversions.RichExpr
import jurisk.optimization.Optimizer
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.math.Fractional.Implicits.infixFractionalOps

object Advent24 {
  final case class PositionAndVelocity2D(
    position: Coordinates2D[Long],
    velocity: Coordinates2D[Long],
  )

  final case class PositionAndVelocity3D(
    position: Coords3D[Long],
    velocity: Coords3D[Long],
  ) {
    def p: Coords3D[Long] = position
    def v: Coords3D[Long] = velocity
  }

  type InputPart2 = List[PositionAndVelocity3D]

  def parse(input: String): InputPart2 = {
    def parse3D(input: String): PositionAndVelocity3D =
      input match {
        case s"$position @ $velocity" =>
          PositionAndVelocity3D(
            Coords3D.parse[Long](position),
            Coords3D.parse[Long](velocity),
          )
        case _                        => input.failedToParse
      }

    input.parseLines(parse3D)
  }

  def areVectorsParallel(a: Coords3D[Long], b: Coords3D[Long]): Boolean = {
    val ax = BigDecimal(a.x)
    val ay = BigDecimal(a.y)
    val az = BigDecimal(a.z)

    val bx = BigDecimal(b.x)
    val by = BigDecimal(b.y)
    val bz = BigDecimal(b.z)

    List(ax / bx, ay / by, az / bz).distinct.size == 1
  }

  // https://en.wikipedia.org/wiki/Chinese_remainder_theorem
  // From https://github.com/ellentari/aoc2023/blob/main/src/main/scala/aoc/Day24.scala
  // and https://github.com/ellentari/aoc2023/blob/main/src/main/scala/aoc/util/ChineseRemainderTheorem.scala
  def solve2UsingChineseRemainderTheorem(
    data: List[PositionAndVelocity3D]
  ): Long = {
    println("For getting to Chinese Remainder Theorem:")
    println("v = vx + vy + vz")
    println("p = px + py + pz")
    println()

    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      val rp  = r.p.x + r.p.y + r.p.z
      val rv  = r.v.x + r.v.y + r.v.z
      println(s"p ${sgn(-rp)} = (${sgn(rv)} -v) * t$idx")
      println(" ==> ")
      // https://en.wikipedia.org/wiki/Modular_arithmetic#Congruence
      println(s"p ≡ $rp (mod ($rv - v))")
      println()
    }

    // We can assume that rv is rather small, e.g. -1000 to +1000.
    // Then we can solve CRT for various assumed `rv` and IF they are `coprime` (requirement for CRT)
    // then the solution of CRT will be a solution for all the equations and thus the answer

    "Not implemented".fail
  }

  def solve2InferringVelocity(
    data: List[PositionAndVelocity3D]
  ): PositionAndVelocity3D = {
    val debug = false

    if (debug) printEquations(data)

    val velocity = inferVelocity(data)

    def solveAssumingV(
      data: List[PositionAndVelocity3D],
      v: Coords3D[Long],
    ): Coords3D[Long] = {
      if (debug) {
        println(
          s"Now that we know (vx, vy, vz) == $v it becomes a much simpler task"
        )
        data.zipWithIndex foreach { case (r, id) =>
          val idx = id + 1
          println(s"px = ${r.p.x} ${sgn(r.v.x - v.x)} * t$idx")
          println(s"py = ${r.p.y} ${sgn(r.v.y - v.y)} * t$idx")
          println(s"pz = ${r.p.z} ${sgn(r.v.z - v.z)} * t$idx")
          println()
        }
      }

      // TODO: Implement Gaussian reduction (just first 3 equations should be enough)

      // This is linear now so you can use Gaussian reduction to get:
      // t1 = 94255352940 and t2 = 810431007754 and t3 = 857431055888

      // Now just plug it in:
      // 191146615936494 + 342596108503183 + 131079628110881 = 664822352550558

      ???
    }

    val position = solveAssumingV(data, velocity)

    PositionAndVelocity3D(
      position,
      velocity,
    )
  }

  // https://math.stackexchange.com/a/697278
  def linesIntersect(
    a: PositionAndVelocity3D,
    b: PositionAndVelocity3D,
  ): Boolean = {
    val cp    = a.velocity crossProduct b.velocity
    val pDiff = a.position - b.position

    (cp dotProduct pDiff) == 0
  }

  private def lineLineIntersectionGivenTwoPointsOnEachLine[N: Fractional](
    a1: Coordinates2D[N],
    a2: Coordinates2D[N],
    b1: Coordinates2D[N],
    b2: Coordinates2D[N],
  ): Option[Coordinates2D[N]] = {
    val Zero = implicitly[Fractional[N]].zero

    val x1 = a1.x
    val y1 = a1.y
    val x2 = a2.x
    val y2 = a2.y
    val x3 = b1.x
    val y3 = b1.y
    val x4 = b2.x
    val y4 = b2.y

    val bottom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if (bottom == Zero) {
      none
    } else {
      val pxTop =
        (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)

      val px = pxTop / bottom

      val pyTop =
        (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
      val py    = pyTop / bottom

      Coordinates2D(px, py).some
    }
  }

  // TODO: You can rewrite using determinants, possibly change the signature too
  private def vectorIntersection2D(
    a: PositionAndVelocity2D,
    b: PositionAndVelocity2D,
  ): Option[Coordinates2D[BigDecimal]] = {
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

    val result = lineLineIntersectionGivenTwoPointsOnEachLine(
      a.position.map(BigDecimal(_)),
      (a.position + a.velocity).map(BigDecimal(_)),
      b.position.map(BigDecimal(_)),
      (b.position + b.velocity).map(BigDecimal(_)),
    )

    result.flatMap { result =>
      val px = result.x
      val py = result.y

      val Eps = 0.001
      val tax = (px - a.position.x) / a.velocity.x
      val tay = (py - a.position.y) / a.velocity.y
      assert((tax - tay).abs <= Eps)
      val ta  = (tax + tay) / 2

      val tbx = (px - b.position.x) / b.velocity.x
      val tby = (py - b.position.y) / b.velocity.y
      assert((tbx - tby).abs <= Eps)
      val tb  = (tbx + tby) / 2

      if (ta > 0 && tb > 0) {
        Coordinates2D[BigDecimal](px, py).some
      } else {
        none
      }
    }
  }

  def part1(data: InputPart2, minC: Long, maxC: Long): Long = {
    val area =
      Area2D[BigDecimal](
        Coordinates2D[BigDecimal](minC, minC),
        Coordinates2D[BigDecimal](maxC, maxC),
      )

    val input = data.map { c =>
      PositionAndVelocity2D(
        Coordinates2D[Long](
          c.position.x,
          c.position.y,
        ),
        Coordinates2D[Long](
          c.velocity.x,
          c.velocity.y,
        ),
      )
    }

    def solve1(
      input: List[PositionAndVelocity2D],
      area: Area2D[BigDecimal],
    ): Long = {
      def intersectWithinBounds2D(
        a: PositionAndVelocity2D,
        b: PositionAndVelocity2D,
      ): Boolean = {
        val debug = false

        if (debug) println(s"a = $a, b= $b")
        val result = vectorIntersection2D(a, b)
        if (debug) println(s"result = $result\n")

        result match {
          case Some(c) =>
            area.contains(c)
          case None    => false
        }
      }

      input.combinations(2).count { list =>
        val List(a, b) = list
        intersectWithinBounds2D(a, b)
      }
    }

    solve1(input, area)
  }

  private def sgn(n: Long): String =
    if (n < 0) {
      s"- ${-n}"
    } else {
      s"+ $n"
    }

  private def toBasicEquation(
    r: PositionAndVelocity3D,
    idx: String,
    axis: Axis,
  ): String = {
    val rp = r.p(axis)
    val rv = r.v(axis)
    val a  = axis.toChar

    s"p$a ${sgn(-rp)} = t$idx * ($rv - v$a)"
  }

  private def inferVelocity(
    data: List[PositionAndVelocity3D]
  ): Coords3D[Long] = {
    def deriveV(axis: Axis): Long = {
      val debug                         = false
      var candidates: Option[Set[Long]] = None

      if (debug) println(s"Same r.v.${axis.toChar}: ")
      data
        .groupBy(_.v(axis))
        .filter { case (_, equations) => equations.size >= 2 }
        .foreach { case (n, list) =>
          if (debug) {
            list.zipWithIndex.foreach { case (r, idx) =>
              println(toBasicEquation(r, ('a' + idx).toString, axis))
            }
          }

          list.combinations(2) foreach { list2 =>
            val List(a, b) = list2
            val rpDiff     = (a.p(axis) - b.p(axis)).abs

            assert(n == a.v(axis))
            assert(n == b.v(axis))

            val divisors    = positiveAndNegativeDivisors(rpDiff)
            if (debug)
              println(s"($n - v${axis.toChar}) is one of $divisors, thus...")
            val validValues = divisors.map(n - _)
            if (debug) println(s"v${axis.toChar} is one of $validValues")

            candidates match {
              case Some(filtered) =>
                val n = filtered intersect validValues.toSet
                candidates = n.some
              case None           => candidates = validValues.toSet.some
            }
          }
          if (debug) println()
        }

      if (debug) {
        println(s"Outcome: Valid v${axis.toChar}-es: $candidates")
        println()
      }

      candidates.get.toSeq.singleResultUnsafe
    }

    val vx = deriveV(Axis.X)
    val vy = deriveV(Axis.Y)
    val vz = deriveV(Axis.Z)

    println(s"v = $vx, $vy, $vz")

    Coords3D[Long](vx, vy, vz)
  }

  def printEquations(data: List[PositionAndVelocity3D]): Unit = {
    println("Basic equations:")
    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      println(s"px + t$idx * vx = ${r.p.x} ${sgn(r.v.x)} * t$idx")
      println(s"py + t$idx * vy = ${r.p.y} ${sgn(r.v.y)} * t$idx")
      println(s"pz + t$idx * vz = ${r.p.z} ${sgn(r.v.z)} * t$idx")
      println()
    }

    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      Axis.All foreach { axes =>
        println(toBasicEquation(r, idx.toString, axes))
      }

      println()
    }

    println()

    println(s"${3 + 3 + data.length} variables")
    println(s"${data.length * 3} equations")

    println()
    println("px + t_n * vx = rpx + rvx * t_n")
    println("py + t_n * vy = rpy + rvy * t_n")
    println("pz + t_n * vz = rpz + rvz * t_n")

    println()
  }

  // TODO: `z3-turnkey` doesn't work here, switch to the command line version
  def solvePart2Optimizer(
    data: List[PositionAndVelocity3D]
  ): PositionAndVelocity3D = {
    // Find a "result" PositionAndVelocity3D for which integer t exists where "position at t" for both
    // "result" and all of "data" is identical

    // t[n] - collision time with rock n, > 0

    // find (px, py, pz) and (vx, vy, vz) and such t[] so that for all rocks:
    //    px + t[n] * vx == px[n] + t[n] * vx[n]
    //    py + t[n] * vy == py[n] + t[n] * vy[n]
    //    pz + t[n] * vz == pz[n] + t[n] * vz[n]

    implicit val o = Optimizer.z3()
    println(Version.getFullVersion)
    import o._

    val px = o.labeledInt("px")
    val py = o.labeledInt("py")
    val pz = o.labeledInt("pz")

    val vx = o.labeledInt("vx")
    val vy = o.labeledInt("vy")
    val vz = o.labeledInt("vz")

    data.zipWithIndex.foreach { case (rock, idx) =>
      val t_n  = o.labeledInt(s"t_$idx")
      val px_n = o.longConstant(rock.position.x)
      val py_n = o.longConstant(rock.position.y)
      val pz_n = o.longConstant(rock.position.z)

      val vx_n = o.longConstant(rock.velocity.x)
      val vy_n = o.longConstant(rock.velocity.y)
      val vz_n = o.longConstant(rock.velocity.z)

      o.addConstraints(
        t_n >= Zero,
        px + t_n * vx === px_n + t_n * vx_n,
        py + t_n * vy === py_n + t_n * vy_n,
        pz + t_n * vz === pz_n + t_n * vz_n,
      )
    }

    println(o.optimize)

    println("""
              |(echo "position:")
              |(eval px) (eval py) (eval pz)
              |
              |(echo "velocity:")
              |(eval vx) (eval vy) (eval vz)
              |
              |(echo "answer:")
              |(eval (+ px py pz))
              |""".stripMargin)

    val model = o.checkAndGetModel()

    println(model)

    val result = PositionAndVelocity3D(
      Coords3D[Long](o.extractInt(px), o.extractInt(py), o.extractInt(pz)),
      Coords3D[Long](o.extractInt(vx), o.extractInt(vy), o.extractInt(vz)),
    )

    println(result)

    result
  }

  def part2(data: InputPart2): Long = {
    // TODO: Consider also trying Newton-Raphson and/or gradient descent
    val result = solve2InferringVelocity(data)
    result.position.x + result.position.y + result.position.z
  }

  def parseFile(fileName: String): InputPart2 =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2023/24$suffix.txt"

  def main(args: Array[String]): Unit = {
    val realData: InputPart2 = parseFile(fileName(""))

    println(s"Part 1: ${part1(realData, 200000000000000L, 400000000000000L)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
