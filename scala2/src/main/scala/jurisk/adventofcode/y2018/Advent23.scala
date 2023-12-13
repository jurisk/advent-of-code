package jurisk.adventofcode.y2018

import com.microsoft.z3.{BoolSort, Context, Expr, IntNum, IntSort}
import jurisk.geometry.{Area3D, Coords3D}
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent23 {
  type Input = List[Nanobot]

  final case class Nanobot(
    position: Coords3D,
    radius: Int,
  )

  object Nanobot {
    def parse(s: String): Nanobot = {
      val RegEx = """pos=<([-+]?\d+),([-+]?\d+),([-+]?\d+)>, r=(\d+)""".r

      s match {
        case RegEx(x, y, z, r) =>
          Nanobot(Coords3D(x.toInt, y.toInt, z.toInt), r.toInt)
        case _                 => s.failedToParse
      }
    }
  }

  def parse(input: String): Input =
    input.parseLines(Nanobot.parse)

  def part1(data: Input): Int = {
    val strongest = data.maxBy(_.radius)
    data.count(x =>
      (x.position manhattanDistance strongest.position) <= strongest.radius
    )
  }

  def part2(data: Input): Int = {
    data foreach println
    println()

    val ctx = new Context
    import ctx._

    val o = mkOptimize

    val List(x, y, z) = List("x", "y", "z").map(mkIntConst)

    val (minCoord, maxCoord) = {
      val boundingBox = Area3D.boundingBoxInclusive(data.map(_.position))
      val min         = boundingBox.min.x min boundingBox.min.y min boundingBox.min.z
      val max         = boundingBox.max.x max boundingBox.max.y max boundingBox.max.z
      (mkInt(min), mkInt(max))
    }

    o.Add(
      mkGe(x, minCoord),
      mkGe(y, minCoord),
      mkGe(z, minCoord),
      mkLe(x, maxCoord),
      mkLe(y, maxCoord),
      mkLe(z, maxCoord),
    )

    val Zero     = mkInt(0)
    val One      = mkInt(1)
    val MinusOne = mkInt(-1)

    def abs(v: Expr[IntSort]): Expr[IntSort] =
      mkITE(mkGe(v, Zero), v, mkMul(v, MinusOne))

    def boolToInt(b: Expr[BoolSort]): Expr[IntSort] =
      mkITE(b, One, Zero)

    def nanobotInRange(nanobot: Nanobot): Expr[IntSort] = {
      val List(nx, ny, nz, nr) = List(
        nanobot.position.x,
        nanobot.position.y,
        nanobot.position.z,
        nanobot.radius,
      ).map(v => mkInt(v))

      val xe = mkSub(x, nx)
      val ye = mkSub(y, ny)
      val ze = mkSub(z, nz)

      val inRange = mkLe(
        mkAdd(
          Array(xe, ye, ze).map(abs): _*
        ),
        nr,
      )

      boolToInt(inRange)
    }

    val nanobotsInRange = mkIntConst("nanobotsInRange")
    o.Add(mkEq(nanobotsInRange, mkAdd(data.map(nanobotInRange).toArray: _*)))

    val distanceFromOrigin = mkIntConst("distanceFromOrigin")
    o.Add(mkEq(distanceFromOrigin, mkAdd(abs(x), abs(y), abs(z))))

    // Objective - maximize nanobotsInRange and minimize distanceFromOrigin
    val objective1 = o.MkMaximize(nanobotsInRange)
    val objective2 = o.MkMinimize(distanceFromOrigin)

    println(o)
    println(o.Check())

    println(objective1.getLower)
    println(objective1.getUpper)
    println(objective1)

    println(objective2.getLower)
    println(objective2.getUpper)
    println(objective2)

    val model = o.getModel

    println(model)

    val List(xc, yc, zc, nir, dor) =
      List(x, y, z, nanobotsInRange, distanceFromOrigin).map { v =>
        val result = model.evaluate(v, true)
        result match {
          case intNum: IntNum => intNum.getInt
          case _              => s"Expected IntNum: $result".fail
        }
      }

    val found = Coords3D(xc, yc, zc)
    println(s"$found: nanobots in range: $nir, distance from origin: $dor")
    found.manhattanDistance(Coords3D.Zero)
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2018/23.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
