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
  )

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

  def textually(data: List[PositionAndVelocity3D]): String = {
    val vx = "a"
    val vy = "b"
    val vz = "c"

    val px = "d"
    val py = "e"
    val pz = "f"

//    val vx = "vx"
//    val vy = "vy"
//    val vz = "vz"
//
//    val px = "px"
//    val py = "py"
//    val pz = "pz"

    def cp(
      a: Coordinates3D,
      bx: String,
      by: String,
      bz: String,
    ): (String, String, String) =
      (
        s"(${a.y}) * $bz - (${a.z}) * $by",
        s"(${a.z}) * $bx - (${a.x}) * $bz",
        s"(${a.x}) * $by - (${a.y}) * $bx",
      )

    def sb(
      a: Coordinates3D,
      bx: String,
      by: String,
      bz: String,
    ): (String, String, String) =
      (
        s"(${a.x}) - $bx",
        s"(${a.y}) - $by",
        s"(${a.z}) - $bz",
      )

    def dp(a: (String, String, String), b: (String, String, String)) = {
      val (ax, ay, az) = a
      val (bx, by, bz) = b
      s"$ax * $bx + $ay * $by + $az * $bz"
    }

    data
      .map { rock =>
        s"${dp(cp(rock.velocity, vx, vy, vz), sb(rock.position, px, py, pz))} = 0"
      }
      .mkString("\n")
  }

  def solvePart2(data: List[PositionAndVelocity3D]): Coordinates3D = {
    println(textually(data))
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

    val Limit = 25
//    val Limit = 1_000_000_000_000L
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
    println(s"velocity = $velocity")

    val result =
      Coordinates3D(o.extractInt(px), o.extractInt(py), o.extractInt(pz))
    println(s"Result = $result")

    result
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
    data foreach println

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

    PositionAndVelocity3D(
      Coordinates3D(o.extractInt(px), o.extractInt(py), o.extractInt(pz)),
      Coordinates3D(o.extractInt(vx), o.extractInt(vy), o.extractInt(vz)),
    )
  }

  def part2(data: InputPart2): Long = {
    val result = solvePart2(data)
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
