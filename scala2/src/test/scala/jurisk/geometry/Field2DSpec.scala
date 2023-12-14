package jurisk.geometry

import jurisk.geometry.Rotation.{Left90, NoRotation, Right90, TurnAround}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Field2DSpec extends AnyFreeSpec {
  private val field_12_34 = Field2D.parseDigitField("""12
                                                      |34
                                                      |""".stripMargin)

  "flatMap" in {
    val result = field_12_34.flatMap { x =>
      Field2D(
        Vector(
          Vector(x, x * 2, x * 3),
          Vector(x * 4, x * 5, x * 6),
        )
      )
    }

    val expected = Field2D(
      Vector(
        Vector(1, 2, 3, 2, 4, 6),
        Vector(4, 5, 6, 8, 10, 12),
        Vector(3, 6, 9, 4, 8, 12),
        Vector(12, 15, 18, 16, 20, 24),
      )
    )

    result shouldEqual expected
  }

  "rotate" - {
    "NoRotation" in {
      field_12_34.rotate(NoRotation) shouldEqual Field2D(
        Vector(Vector(1, 2), Vector(3, 4))
      )
    }

    "Left90" in {
      field_12_34.rotate(Left90) shouldEqual Field2D(
        Vector(Vector(2, 4), Vector(1, 3))
      )
    }

    "TurnAround" in {
      field_12_34.rotate(TurnAround) shouldEqual Field2D(
        Vector(Vector(4, 3), Vector(2, 1))
      )
    }

    "Right90" in {
      field_12_34.rotate(Right90) shouldEqual Field2D(
        Vector(Vector(3, 1), Vector(4, 2))
      )
    }

    List(
      List(NoRotation),
      List(Left90, Left90, Left90, Left90),
      List(Right90, Right90, Right90, Right90),
      List(Left90, Left90, TurnAround),
      List(Right90, Right90, TurnAround),
    ) foreach { test =>
      s"rotate $test" in {
        test.foldLeft(field_12_34) { case (acc, r) =>
          acc.rotate(r)
        } shouldEqual field_12_34
      }
    }
  }
}
