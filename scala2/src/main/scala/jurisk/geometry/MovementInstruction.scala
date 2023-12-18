package jurisk.geometry

import cats.data.NonEmptyList
import jurisk.geometry.Direction2D.CardinalDirection2D

import scala.annotation.tailrec

final case class MovementInstruction(
  direction: CardinalDirection2D,
  distance: Int,
) {
  def applyTo(coords: Coords2D): Coords2D =
    coords + direction.diff * distance
}

object MovementInstruction {
  def walkEveryPoint(
    instructions: Seq[MovementInstruction],
    start: Coords2D = Coords2D.Zero,
  ): IndexedSeq[Coords2D] = {
    val flattened = instructions flatMap { instruction =>
      Seq.fill(instruction.distance)(
        MovementInstruction(instruction.direction, 1)
      )
    }

    walkPath(flattened, start)
  }

  def walkPath(
    instructions: Seq[MovementInstruction],
    start: Coords2D = Coords2D.Zero,
  ): IndexedSeq[Coords2D] = {
    @tailrec
    def f(
      acc: NonEmptyList[Coords2D],
      rem: List[MovementInstruction],
    ): NonEmptyList[Coords2D] =
      rem match {
        case head :: tail =>
          val next = head.applyTo(acc.head)
          f(next :: acc, tail)
        case Nil          => acc.reverse
      }

    IndexedSeq.from(f(NonEmptyList.one(start), instructions.toList).toList)
  }
}
