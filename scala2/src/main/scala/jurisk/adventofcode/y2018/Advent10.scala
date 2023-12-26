package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.geometry.Coordinates2D
import jurisk.geometry.Coords2D
import jurisk.geometry.SparseBooleanField
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

object Advent10 {
  type Parsed  = List[PointWithVelocity]
  type Result1 = Long

  final case class PointWithVelocity(
    location: Coords2D,
    velocity: Coords2D,
  ) {
    def atTime(time: Int): Coords2D = location + velocity * time
  }

  object PointWithVelocity {
    def parse(s: String): PointWithVelocity = {
      val List(a, b, c, d) = s.extractIntList
      PointWithVelocity(Coords2D.of(a, b), Coords2D.of(c, d))
    }
  }

  def parse(data: String): Parsed =
    data.parseLines(PointWithVelocity.parse)

  def solve(data: Parsed, limit: Int): Result1 = {
    val field = data.toSet

    Simulation.runWithIterationCount(()) { case ((), time) =>
      val atTime      = field.map(x => x.atTime(time.toInt))
      val boundingBox = Coordinates2D.boundingBoxInclusive(atTime)
      if (boundingBox.height <= limit) {
        val fieldAtTime = SparseBooleanField(atTime, 100).toDebugRepresentation
        println(s"At time $time:")
        println(fieldAtTime)
        time.asLeft
      } else {
        ().asRight
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val testData = readFileText("2018/10-test.txt")
    val realData = readFileText("2018/10.txt")

    val test = parse(testData)
    val real = parse(realData)

    solve(test, 8) shouldEqual 3
    solve(real, 12) shouldEqual 10519
  }
}
