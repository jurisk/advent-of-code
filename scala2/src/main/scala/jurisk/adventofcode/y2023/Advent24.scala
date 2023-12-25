package jurisk.adventofcode.y2023

import cats.implicits.{catsSyntaxOptionId, none}
import com.microsoft.z3.Version
import jurisk.math.divisors
import jurisk.optimization.ImplicitConversions.{RichArithExprIntSort, RichExpr}
import jurisk.optimization.Optimizer
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent24 {
  // TODO: Move to Coords2D
  final case class Coordinates2D[T](
    x: T,
    y: T,
  )

  final case class PositionAndVelocity2D(
    position: Coordinates2D[BigDecimal],
    velocity: Coordinates2D[BigDecimal],
  )

  // TODO: Move / merge to Coords3D
  final case class Coordinates3D(
    x: Long,
    y: Long,
    z: Long,
  ) {
    def get(axis: Axis): Long = axis match {
      case Axis.X => x
      case Axis.Y => y
      case Axis.Z => z
    }

    def -(other: Coordinates3D): Coordinates3D =
      Coordinates3D(x - other.x, y - other.y, z - other.z)
  }

  object Coordinates3D {
    def parse(s: String): Coordinates3D = s match {
      case s"$x,$y,$z" =>
        Coordinates3D(x.trim.toLong, y.trim.toLong, z.trim.toLong)
      case _           => s.failedToParse("Coordinates3D")
    }
  }

  final case class PositionAndVelocity3D(
    position: Coordinates3D,
    velocity: Coordinates3D,
  ) {
    def v: Coordinates3D = velocity
    def p: Coordinates3D = position
  }

  type InputPart2 = List[PositionAndVelocity3D]

  def parse3D(input: String): PositionAndVelocity3D =
    input match {
      case s"$position @ $velocity" =>
        PositionAndVelocity3D(
          Coordinates3D.parse(position),
          Coordinates3D.parse(velocity),
        )
      case _                        => input.failedToParse
    }

  def parse(input: String): InputPart2 =
    input.parseLines(parse3D)

  def crossProduct(a: Coordinates3D, b: Coordinates3D): Coordinates3D =
    Coordinates3D(
      a.y * b.z - a.z * b.y,
      a.z * b.x - a.x * b.z,
      a.x * b.y - a.y * b.x,
    )

  def dotProduct(a: Coordinates3D, b: Coordinates3D): Long =
    a.x * b.x + a.y * b.y + a.z * b.z

  def areVectorsParallel(a: Coordinates3D, b: Coordinates3D): Boolean = {
    val ax = BigDecimal(a.x)
    val ay = BigDecimal(a.y)
    val az = BigDecimal(a.z)

    val bx = BigDecimal(b.x)
    val by = BigDecimal(b.y)
    val bz = BigDecimal(b.z)

    List(ax / bx, ay / by, az / bz).distinct.size == 1
  }

  def solvePart2(data: List[PositionAndVelocity3D]): PositionAndVelocity3D = {
    printEquations(data)

    val velocity = calculateVelocity(data)
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
    val cp    = crossProduct(a.velocity, b.velocity)
    val pDiff = a.position - b.position

    dotProduct(cp, pDiff) == 0
  }

  private def vectorIntersection2D(
    a: PositionAndVelocity2D,
    b: PositionAndVelocity2D,
  ): Option[Coordinates2D[BigDecimal]] = {
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line

    val x1: BigDecimal = a.position.x
    val y1: BigDecimal = a.position.y
    val x2: BigDecimal = a.position.x + a.velocity.x
    val y2: BigDecimal = a.position.y + a.velocity.y

    val x3: BigDecimal = b.position.x
    val y3: BigDecimal = b.position.y
    val x4: BigDecimal = b.position.x + b.velocity.x
    val y4: BigDecimal = b.position.y + b.velocity.y

    val bottom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if (bottom == 0) {
      none
    } else {
      val pxTop =
        (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)

      val px = pxTop / bottom

      val pyTop =
        (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
      val py    = pyTop / bottom

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

  def solve1(
    input: List[PositionAndVelocity2D],
    min: Coordinates2D[Long],
    max: Coordinates2D[Long],
  ): Long = {
    def intersectWithinBounds2D(
      a: PositionAndVelocity2D,
      b: PositionAndVelocity2D,
      min: Coordinates2D[Long],
      max: Coordinates2D[Long],
    ): Boolean = {
      println(s"a = $a, b= $b")
      val result = vectorIntersection2D(a, b)
      println(s"result = $result\n")

      result match {
        case Some(c) =>
          val inBounds =
            c.x >= min.x && c.x <= max.x && c.y >= min.y && c.y <= max.y
          inBounds
        case None    => false
      }
    }

    input.combinations(2).count { list =>
      list match {
        case List(a, b) =>
          intersectWithinBounds2D(a, b, min, max)

        case _ =>
          list.toString.fail
      }
    }
  }

  def part1(data: InputPart2, minC: Long, maxC: Long): Long = {
    val min   = Coordinates2D[Long](minC, minC)
    val max   = Coordinates2D[Long](maxC, maxC)
    val input = data.map { c =>
      PositionAndVelocity2D(
        Coordinates2D[BigDecimal](
          BigDecimal.decimal(c.position.x),
          BigDecimal.decimal(c.position.y),
        ),
        Coordinates2D[BigDecimal](
          BigDecimal.decimal(c.velocity.x),
          BigDecimal.decimal(c.velocity.y),
        ),
      )
    }

    solve1(input, min, max)
  }

  sealed trait Axis {
    def toChar: Char
  }
  object Axis       {
    case object X extends Axis {
      override def toChar: Char = 'x'
    }
    case object Y extends Axis {
      override def toChar: Char = 'y'
    }
    case object Z extends Axis {
      override def toChar: Char = 'z'
    }

    val All: Set[Axis] = Set(X, Y, Z)
  }

  private def sgn(n: Long): String =
    if (n < 0) {
      s"- ${-n}"
    } else {
      s"+ $n"
    }

  def solveAssumingV(
    data: List[PositionAndVelocity3D],
    v: Coordinates3D,
  ): Coordinates3D = {
    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      println(s"px  = ${r.p.x} ${sgn(r.v.x - v.x)} * t$idx")
      println(s"py = ${r.p.y} ${sgn(r.v.y - v.y)} * t$idx")
      println(s"pz = ${r.p.z} ${sgn(r.v.z - v.z)} * t$idx")
      println()
    }

    // This is linear now so you can use Gaussian reduction to get:
    // t1 = 94255352940 and t2 = 810431007754 and t3 = 857431055888

    // Now just plug it in:
    // 191146615936494 + 342596108503183 + 131079628110881 = 664822352550558

    ???
  }

  def calculateVelocity(data: List[PositionAndVelocity3D]): Coordinates3D = {
    def deriveV(axis: Axis): Long = {
      val debug                         = false
      var candidates: Option[Set[Long]] = None

      if (debug) println(s"Same r.v.${axis.toChar}: ")
      data.groupBy(_.v.get(axis)).filter(_._2.size >= 2).foreach {
        case (n, list) =>
          list.combinations(2) foreach { list2 =>
            val List(a, b) = list2
            val rpDiff     = (a.p.get(axis) - b.p.get(axis)).abs

            assert(n == a.v.get(axis))
            assert(n == b.v.get(axis))

            val temp        = divisors(rpDiff)
            val divs        = temp ++ temp.map(-_)
            if (debug)
              println(s"($n - v${axis.toChar}) is one of $divs, thus...")
            val validValues = divs.map(n - _)
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

      println(s"Outcome: Valid v${axis.toChar}-es: $candidates")
      println()

      candidates.get.toSeq.singleResultUnsafe
    }

    val vx = deriveV(Axis.X)
    val vy = deriveV(Axis.Y)
    val vz = deriveV(Axis.Z)

    println(s"v = $vx, $vy, $vz")

    Coordinates3D(vx, vy, vz)
  }

  def printEquations(data: List[PositionAndVelocity3D]): Unit = {
    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      println(s"px + t$idx * vx = ${r.p.x} ${sgn(r.v.x)} * t$idx")
      println(s"py + t$idx * vy = ${r.p.y} ${sgn(r.v.y)} * t$idx")
      println(s"pz + t$idx * vz = ${r.p.z} ${sgn(r.v.z)} * t$idx")
      println()
    }

    def printNice(r: PositionAndVelocity3D, idx: Int, axis: Axis): String = {
      val rp = r.p.get(axis)
      val rv = r.v.get(axis)
      val a  = axis.toChar

      s"p$a ${sgn(-rp)} = t$idx * ($rv - v$a)"
    }

    data.zipWithIndex foreach { case (r, id) =>
      val idx = id + 1
      Axis.All foreach { axes =>
        printNice(r, idx, axes)
      }

      println()
    }

    println()

    println(s"${3 + 3 + data.length} variables")
    println(s"${data.length * 3} equations")

    println()
    println(s"px + t_n * vx = rpx + rvx * t_n")
    println(s"py + t_n * vy = rpy + rvy * t_n")
    println(s"pz + t_n * vz = rpz + rvz * t_n")

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

    val px = o.labeledInt(s"px")
    val py = o.labeledInt(s"py")
    val pz = o.labeledInt(s"pz")

    val vx = o.labeledInt(s"vx")
    val vy = o.labeledInt(s"vy")
    val vz = o.labeledInt(s"vz")

    data.zipWithIndex.foreach { case (rock, idx) =>
      val t_n  = o.labeledInt(s"t_$idx")
      val px_n = o.constant(rock.position.x)
      val py_n = o.constant(rock.position.y)
      val pz_n = o.constant(rock.position.z)

      val vx_n = o.constant(rock.velocity.x)
      val vy_n = o.constant(rock.velocity.y)
      val vz_n = o.constant(rock.velocity.z)

      o.addConstraints(
        t_n >= Zero,
        px + t_n * vx === px_n + t_n * vx_n,
        py + t_n * vy === py_n + t_n * vy_n,
        pz + t_n * vz === pz_n + t_n * vz_n,
      )
    }

    println(o.optimize)

    println(s"""
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
      Coordinates3D(o.extractInt(px), o.extractInt(py), o.extractInt(pz)),
      Coordinates3D(o.extractInt(vx), o.extractInt(vy), o.extractInt(vz)),
    )

    println(result)

    result
  }

  def part2(data: InputPart2): Long = {
    val result = solvePart2(data).position
    result.x + result.y + result.z
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
