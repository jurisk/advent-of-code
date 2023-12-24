package jurisk.adventofcode.y2023

import cats.implicits.catsSyntaxOptionId
import cats.implicits.none
import com.microsoft.z3.{ArithExpr, IntExpr, IntSort}
import jurisk.optimization.ImplicitConversions.{
  RichArithExprIntSort,
  RichExpr,
  RichLong,
}
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent24 {
  final case class Coordinates2D[T](
    x: T,
    y: T,
  )

  final case class PositionAndVelocity2D(
    position: Coordinates2D[BigDecimal],
    velocity: Coordinates2D[BigDecimal],
  )

  final case class Coordinates3D(
    x: Long,
    y: Long,
    z: Long,
  ) {
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

  def approximatelyEquals(
    a: Double,
    b: Double,
    tolerance: Double = 1e-6,
  ): Boolean =
    math.abs(a - b) < tolerance

  def crossProduct(a: Coordinates3D, b: Coordinates3D): Coordinates3D =
    Coordinates3D(
      a.y * b.z - a.z * b.y,
      a.z * b.x - a.x * b.z,
      a.x * b.y - a.y * b.x,
    )

  def dotProduct(a: Coordinates3D, b: Coordinates3D): Long =
    a.x * b.x + a.y * b.y + a.z * b.z

  def textually(data: List[PositionAndVelocity3D], num: Boolean): String = {
    val vx = "A"
    val vy = "B"
    val vz = "C"

    val px = "X"
    val py = "Y"
    val pz = "Z"

//    val vx = "vx"
//    val vy = "vy"
//    val vz = "vz"
//
//    val px = "px"
//    val py = "py"
//    val pz = "pz"

    def cp(
      a: (String, String, String),
      b: (String, String, String),
    ): (String, String, String) = {
      val (ax, ay, az) = a
      val (bx, by, bz) = b
      (
        s"($ay * $bz - $az * $by)",
        s"($az * $bx - $ax * $bz)",
        s"($ax * $by - $ay * $bx)",
      )
    }

    def sb(
      a: (String, String, String),
      b: (String, String, String),
    ): (String, String, String) = {

      val (ax, ay, az) = a
      val (bx, by, bz) = b

      (
        s"($ax - $bx)",
        s"($ay - $by)",
        s"($az - $bz)",
      )
    }

    def dp(a: (String, String, String), b: (String, String, String)) = {
      val (ax, ay, az) = a
      val (bx, by, bz) = b
      s"$ax * $bx + $ay * $by + $az * $bz"
    }

    def extractStrings(c: Coordinates3D): (String, String, String) =
      (
        s"${c.x}",
        s"${c.y}",
        s"${c.z}",
      )

    data
      .map { rock =>
        if (num) {
          s"${dp(cp(extractStrings(rock.velocity), (vx, vy, vz)), sb(extractStrings(rock.position), (px, py, pz)))} = 0"
        } else {
//          s"${dp(cp(("rvx", "rvy", "rvz"), (vx, vy, vz)), sb(("rpx", "rpy", "rpz"), (px, py, pz)))} = 0"
          s"${dp(cp(("d", "f", "g"), (vx, vy, vz)), sb(("h", "j", "k"), (px, py, pz)))} = 0"
        }
      }
      .mkString("\n")

  }

  def areVectorsParallel(a: Coordinates3D, b: Coordinates3D): Boolean = {
    val ax = BigDecimal(a.x)
    val ay = BigDecimal(a.y)
    val az = BigDecimal(a.z)

    val bx = BigDecimal(b.x)
    val by = BigDecimal(b.y)
    val bz = BigDecimal(b.z)

    List(ax / bx, ay / by, az / bz).distinct.size == 1
  }

  def normaliseToAvg(data: List[PositionAndVelocity3D]): Unit = {
    val xs = data.map(_.position.x)
    val ys = data.map(_.position.y)
    val zs = data.map(_.position.z)

    val xAvg = xs.sum / xs.length
    val yAvg = ys.sum / ys.length
    val zAvg = zs.sum / zs.length

    println()
    data foreach { r =>
      val q = r.copy(position =
        Coordinates3D(
          r.position.x - xAvg,
          r.position.y - yAvg,
          r.position.z - zAvg,
        )
      )
      println(s"${q.p.x}, ${q.p.y}, ${q.p.z} @ ${q.v.x}, ${q.v.y}, ${q.v.z}")
    }
    println()
  }

  def solvePart2(data: List[PositionAndVelocity3D]): PositionAndVelocity3D = {
    anyoneIntersecting(data)

//    println(textually(data, num = true))
//    println()
//    println(textually(data, num = false))
//    println()

    // For all rocks "r"
    // (rvy * vz - rvz * vy) * (rpx - px) + (rvz * vx - rvx * vz) * (rpy - py) + (rvx * vy - rvy * vx) * (rpz - pz) = 0

    // (rvy * C - rvz * B) * (rpx - X) + (rvz * A - rvx * C) * (rpy - Y) + (rvx * B - rvy * A) * (rpz - Z) = 0

    solvePart2CrudeOptimize(data)
//    solvePart2DotCrossProducts(data.take(3), 100_000_000_000L)

  }

  def anyoneParallel(data: List[PositionAndVelocity3D]): Unit =
    data.combinations(2).foreach { list =>
      val List(a, b) = list
      if (areVectorsParallel(a.velocity, b.velocity)) {
        println(s"Parallel $a and $b")
      }
    }

  def anyoneIntersecting(data: List[PositionAndVelocity3D]): Unit =
    data.combinations(2).foreach { list =>
      val List(a, b) = list
      if (linesIntersect(a, b)) {
        println(s"Intersect $a and $b")
      }
    }

  def solvePart2DotCrossProducts(
    data: List[PositionAndVelocity3D],
    limit: Long,
  ): PositionAndVelocity3D = {
    println(textually(data, num = true))
    println()
    println(textually(data, num = false))
    println()

    // Find such px, py, pz, vx, vy, vz that for all rocks
    // dotProduct(crossProduct(rock.velocity, p.velocity), rock.position - v.position) == 0

    implicit val o: Optimizer = Optimizer.z3()
    import o._

    val px = o.labeledInt(s"px")
    val py = o.labeledInt(s"py")
    val pz = o.labeledInt(s"pz")

    val vx = o.labeledInt(s"vx")
    val vy = o.labeledInt(s"vy")
    val vz = o.labeledInt(s"vz")

//    val Limit = 25
//    val Limit = 1_000_000_000_000L
    o.addConstraints(
      px <= o.constant(limit),
      py <= o.constant(limit),
      pz <= o.constant(limit),
      px >= o.constant(-limit),
      py >= o.constant(-limit),
      pz >= o.constant(-limit),
      vx <= o.constant(limit),
      vy <= o.constant(limit),
      vz <= o.constant(limit),
      vx >= o.constant(-limit),
      vy >= o.constant(-limit),
      vz >= o.constant(-limit),
    )

    def cp(
      a: Coordinates3D,
      bx: IntExpr,
      by: IntExpr,
      bz: IntExpr,
    ): (ArithExpr[IntSort], ArithExpr[IntSort], ArithExpr[IntSort]) =
      (
        a.y.constant * bz - a.z.constant * by,
        a.z.constant * bx - a.x.constant * bz,
        a.x.constant * by - a.y.constant * bx,
      )

    def sb(
      a: Coordinates3D,
      bx: IntExpr,
      by: IntExpr,
      bz: IntExpr,
    ): (ArithExpr[IntSort], ArithExpr[IntSort], ArithExpr[IntSort]) =
      (
        constant(a.x) - bx,
        constant(a.y) - by,
        constant(a.z) - bz,
      )

    def dp(
      a: (ArithExpr[IntSort], ArithExpr[IntSort], ArithExpr[IntSort]),
      b: (ArithExpr[IntSort], ArithExpr[IntSort], ArithExpr[IntSort]),
    ) = {
      val (ax, ay, az) = a
      val (bx, by, bz) = b
      ax * bx + ay * by + az * bz
    }

    data foreach { rock =>
      o.addConstraints(
        dp(
          cp(rock.velocity, vx, vy, vz),
          sb(rock.position, px, py, pz),
        ) === Zero
      )
    }

    o.addConstraints(
      vx * vx + vy * vy + vz * vz > Zero
    )

    println(o.optimize)

    val model = o.checkAndGetModel()

    println(model)

    val velocity =
      Coordinates3D(o.extractInt(vx), o.extractInt(vy), o.extractInt(vz))
    println(s"Velocity = $velocity")

    val position =
      Coordinates3D(o.extractInt(px), o.extractInt(py), o.extractInt(pz))
    println(s"Position = $position")

    // Note that this can find not just the exact solution but also some others that are collinear vectors (reversed),
    // but may technically not be valid solutions because they have intersections at t < 0

    PositionAndVelocity3D(position, velocity)
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

  type Time = Long
  def lineIntersection3D(
    a: PositionAndVelocity3D,
    b: PositionAndVelocity3D,
  ): Option[(Time, Time, Coordinates3D)] = {
    // x_a = a.position.x + a.velocity.x * t
    // y_a = a.position.y + a.velocity.y * t
    // z_a = a.position.z + a.velocity.z * t

    // x_b = b.position.x + b.velocity.x * s
    // y_b = b.position.y + b.velocity.y * s
    // z_b = b.position.z + b.velocity.z * s

    // Find s and t so that:
    // x_a == x_b
    // y_a == y_b
    // z_a == z_b

    // a.position.x + a.velocity.x * t = b.position.x + b.velocity.x * s
    //    ==>
    // a.velocity.x * t - b.velocity.x * s == b.position.x - a.position.x
    // a.velocity.y * t - b.velocity.y * s == b.position.y - a.position.y
    // a.velocity.z * t - b.velocity.z * s == b.position.z - a.position.z

    val cp    = crossProduct(a.velocity, b.velocity)
    val pDiff = a.position - b.position

    val intersect = dotProduct(cp, pDiff)

    println(s"cp = $cp, pDiff = $pDiff, intersect = $intersect")

    // TODO: could calculate these
    val t = 3
    val s = 2

    println(
      s"${a.velocity.x} * t - ${b.velocity.x} * s = ${b.position.x - a.position.x}"
    )
    println(
      s"${a.velocity.y} * t - ${b.velocity.y} * s = ${b.position.y - a.position.y}"
    )
    println(
      s"${a.velocity.z} * t - ${b.velocity.z} * s = ${b.position.z - a.position.z}"
    )

    assert(a.velocity.x * t - b.velocity.x * s == b.position.x - a.position.x)
    assert(a.velocity.y * t - b.velocity.y * s == b.position.y - a.position.y)
    assert(a.velocity.z * t - b.velocity.z * s == b.position.z - a.position.z)

    ???
  }

  def vectorIntersection2D(
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
      println(s"ta error ${(tax - tay).abs}, tax = $tax, tay = $tay")
      assert((tax - tay).abs <= Eps)
      val ta  = (tax + tay) / 2

      val tbx = (px - b.position.x) / b.velocity.x
      val tby = (py - b.position.y) / b.velocity.y
      println(s"tb error ${(tbx - tby).abs}, tbx = $tbx, tby = $tby")
      assert((tbx - tby).abs <= Eps)
      val tb  = (tbx + tby) / 2

      println(s"ta = $ta, tb = $tb")
      if (ta > 0 && tb > 0) {
        Coordinates2D[BigDecimal](px, py).some
      } else {
        none
      }
    }
  }

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

  def solve1(
    input: List[PositionAndVelocity2D],
    min: Coordinates2D[Long],
    max: Coordinates2D[Long],
  ): Long =
    input.combinations(2).count { list =>
      list match {
        case List(a, b) =>
          intersectWithinBounds2D(a, b, min, max)

        case _ =>
          list.toString.fail
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

  def solvePart2CrudeOptimize(
    data: List[PositionAndVelocity3D]
  ): PositionAndVelocity3D = {
    def sgn(n: Long): String =
      if (n < 0) {
        s"- ${-n}"
      } else {
        s"+ $n"
      }

    data.zipWithIndex foreach { case (r, idx) =>
      println(s"x + t$idx * a = ${r.p.x} ${sgn(r.v.x)} * t$idx")
      println(s"y + t$idx * b = ${r.p.y} ${sgn(r.v.y)} * t$idx")
      println(s"z + t$idx * c = ${r.p.z} ${sgn(r.v.z)} * t$idx")
    }

    println()
    println(s"x + t_n * a = rpx + rvx * t_n")
    println(s"y + t_n * b = rpy + rvy * t_n")
    println(s"z + t_n * c = rpz + rvz * t_n")

    // (t_n * a) - (rvx * t_n) = rpx - x
    // t_n * (a - rvx) = rpx - x
    // t_n = (rpx - x) / (a - rvx)
    // t_n = (rpx - x) / (a - rvx) = (rpy - y) / (b - rvy) = (rpz - z) / (c - rvz)

    // x = 24, y = 13, z = 10, a = -3, b = 1, c = 2

    val x = 24
    val y = 13
    val z = 10
    val a = -3
    val b = 1
    val c = 2

    // (19 - x) / (a + 2) = (13 - y) / (b - 1) = (30 - z) / (c + 2)
    // (18 - x) / (a + 1) = (19 - y) / (b + 1) = (22 - z) / (c + 2)
    // (20 - x) / (a + 2) = (25 - y) / (b + 2) = (34 - z) / (c + 4)
    // (12 - x) / (a + 1) = (31 - y) / (b + 2) = (28 - z) / (c + 1)
    // (20 - x) / (a - 1) = (19 - y) / (b + 5) = (15 - z) / (c + 3)

    data foreach { r =>
      if ((a != r.v.x) && (b != r.v.y) && (c != r.v.z)) {
        val e1 = (r.p.x - x).toDouble / (a - r.v.x)
        val e2 = (r.p.y - y).toDouble / (b - r.v.y)
        val e3 = (r.p.z - z).toDouble / (c - r.v.z)

        println(s"$e1, $e2, $e3")
      }

      println(s"(${r.p.x} - x) / (a ${sgn(-r.v.x)}) = (${r.p.y} - y) / (b ${sgn(
          -r.v.y
        )}) = (${r.p.z} - z) / (c ${sgn(-r.v.z)})")
      println()
    }

    println(s"${3 + 3 + data.length} variables")
    println(s"${data.length * 3} equations")

    println()

    // Find a "result" PositionAndVelocity3D for which integer t exists where "position at t" for both
    // "result" and all of "data" is identical

    // t[n] - collision time with rock n, > 0

    // find (px, py, pz) and (vx, vy, vz) and such t[] so that for all rocks:
    //    px + t[n] * vx == px[n] + t[n] * vx[n]
    //    py + t[n] * vy == py[n] + t[n] * vy[n]
    //    pz + t[n] * vz == pz[n] + t[n] * vz[n]

    implicit val o = Optimizer.z3()
    import o._

    val px = o.labeledInt(s"px")
    val py = o.labeledInt(s"py")
    val pz = o.labeledInt(s"pz")

    val vx = o.labeledInt(s"vx")
    val vy = o.labeledInt(s"vy")
    val vz = o.labeledInt(s"vz")

//    val Limit = 1_000_000_000
    val Limit = 25
    o.addConstraints(
      px <= o.constant(Limit),
      py <= o.constant(Limit),
      pz <= o.constant(Limit),
      px >= o.constant(-Limit),
      py >= o.constant(-Limit),
      pz >= o.constant(-Limit),
      vx <= o.constant(Limit),
      vy <= o.constant(Limit),
      vz <= o.constant(Limit),
      vx >= o.constant(-Limit),
      vy >= o.constant(-Limit),
      vz >= o.constant(-Limit),
    )

    val t = data.indices map { idx =>
      o.labeledInt(s"t_$idx")
    }

    t foreach { t_n =>
      o.addConstraints(
        t_n >= Zero
      )
    }

    data.zipWithIndex.foreach { case (rock, idx) =>
      val t_n  = t(idx)
      val px_n = o.constant(rock.position.x)
      val py_n = o.constant(rock.position.y)
      val pz_n = o.constant(rock.position.z)

      val vx_n = o.constant(rock.velocity.x)
      val vy_n = o.constant(rock.velocity.y)
      val vz_n = o.constant(rock.velocity.z)

      o.addConstraints(
        px + t_n * vx === px_n + t_n * vx_n,
        py + t_n * vy === py_n + t_n * vy_n,
        pz + t_n * vz === pz_n + t_n * vz_n,
        t_n <= constant(Limit),
        t_n >= constant(-Limit),
      )
    }

    // Note - we technically don't NEED to minimize, but it seemed to speed things up
    o.minimize(
      px + py + pz + vz + vy + vz + o.sum(t: _*)
    )

    println(o.optimize)

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
//    result
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
