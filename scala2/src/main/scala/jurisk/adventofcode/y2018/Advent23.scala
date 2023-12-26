package jurisk.adventofcode.y2018

import jurisk.geometry.Area3D
import jurisk.geometry.Coords3D
import jurisk.optimization.ImplicitConversions.RichArithExprIntSort
import jurisk.optimization.ImplicitConversions.RichExpr
import jurisk.optimization.ImplicitConversions.RichExprBoolSort
import jurisk.optimization.ImplicitConversions.RichExprIntSort
import jurisk.optimization.ImplicitConversions.RichInt
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent23 {
  type Input = List[Nanobot]

  final case class Nanobot(
    position: Coords3D[Int],
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
    implicit val optimizer: Optimizer = Optimizer.z3()
    import optimizer._

    val List(x, y, z) = List("x", "y", "z").map(labeledInt)

    val (minCoord, maxCoord) = {
      val boundingBox = Area3D.boundingBoxInclusive(data.map(_.position))
      val min         = boundingBox.min.x min boundingBox.min.y min boundingBox.min.z
      val max         = boundingBox.max.x max boundingBox.max.y max boundingBox.max.z
      (min.constant, max.constant)
    }

    addConstraints(
      x >= minCoord,
      y >= minCoord,
      z >= minCoord,
      x <= maxCoord,
      y <= maxCoord,
      z <= maxCoord,
    )

    def nanobotInRange(nanobot: Nanobot) = {
      val List(nx, ny, nz, nr) = List(
        nanobot.position.x,
        nanobot.position.y,
        nanobot.position.z,
        nanobot.radius,
      ).map(intConstant)

      val inRange = (x - nx).abs + (y - ny).abs + (z - nz).abs <= nr

      inRange.toInt
    }

    val nanobotsInRange    = labeledInt("nanobotsInRange")
    val distanceFromOrigin = labeledInt("distanceFromOrigin")
    addConstraints(
      nanobotsInRange === sum(data.map(nanobotInRange): _*),
      distanceFromOrigin === sum(abs(x), abs(y), abs(z)),
    )

    // Objective - maximize nanobotsInRange and minimize distanceFromOrigin
    val objective1 = maximize(nanobotsInRange)
    val objective2 = minimize(distanceFromOrigin)

    val model = checkAndGetModel()

    println(model)

    val List(xc, yc, zc, nir, dor) =
      List(x, y, z, nanobotsInRange, distanceFromOrigin).map(extractInt)

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
