package jurisk.geometry

import jurisk.geometry.Coords3D.areVectorsParallel
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class Coords3DSpec extends AnyFreeSpec {
  // https://www.mathsisfun.com/algebra/vectors-cross-product.html
  "crossProduct" - {
    (Coords3D[Long](2, 3, 4) crossProduct Coords3D[Long](
      5,
      6,
      7,
    )) shouldEqual Coords3D[Long](
      -3,
      6,
      -3,
    )
  }

  "areVectorsParallel" in {
    val a = Coords3D[Long](5, 2, -1)
    val b = Coords3D[Long](-10, -4, 2)
    areVectorsParallel(a, b) shouldEqual true
    areVectorsParallel(a, Coords3D[Long](-3, 1, 2)) shouldEqual false
  }
}
