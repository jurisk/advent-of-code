package jurisk.geometry

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Field2DSpec extends AnyFreeSpec {
  "flatMap" in {
    val field = Field2D.parseDigitField("""12
                                          |34
                                          |""".stripMargin)

    val result = field.flatMap { x =>
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
}
