package jurisk.adventofcode.y2022

import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D._
import jurisk.geometry.Field2D
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

object Advent23 {
  final case class Elf(
    id: Int,
    location: Coords2D,
  ) {
    def proposal(allCoords: Set[Coords2D], iteration: Int): Elf =
      if (location.adjacent8.forall(c => !allCoords.contains(c))) {
        this
      } else {
        MoveProposal.forIteration(iteration).find {
          _.conditionsSatisfied(location, allCoords)
        } match {
          case Some(proposal) =>
            copy(location = location + proposal.moveTo)
          case None           => this
        }
      }

    def charRep: Char = ('a'.toInt + id).toChar
  }

  final case class MoveProposal(
    considered: Set[Direction2D],
    moveTo: CardinalDirection2D,
  ) {
    def conditionsSatisfied(location: Coords2D, all: Set[Coords2D]): Boolean =
      considered.forall { d =>
        !all.contains(location + d)
      }
  }

  object MoveProposal {
    private val All: ArraySeq[MoveProposal] = ArraySeq(
      MoveProposal(Set(N, NE, NW), N),
      MoveProposal(Set(S, SE, SW), S),
      MoveProposal(Set(W, NW, SW), W),
      MoveProposal(Set(E, NE, SE), E),
    )

    def at(iteration: Int): MoveProposal =
      All(iteration % All.size)

    def forIteration(iteration: Int): List[MoveProposal] =
      (0 to 3).toList map { n => at(iteration + n) }
  }

  final case class State(
    elves: List[Elf]
  ) {
    val allCoords: Set[Coords2D] = elves.map(_.location).toSet

    def next(iteration: Int): State = {
      val proposedElves                     = elves.map(_.proposal(allCoords, iteration))
      val duplicatePositions: Set[Coords2D] = proposedElves
        .map(_.location)
        .counts
        .filter { case (_, v) => v > 1 }
        .keySet
      val newElves                          = (elves zip proposedElves).map { case (oldE, newE) =>
        if (duplicatePositions.contains(newE.location)) {
          oldE
        } else {
          newE
        }
      }
      State(newElves)
    }

    def debugPrint(): Unit = {
      val box       = Coords2D.boundingBoxInclusive(elves.map(_.location))
      val field     = Field2D.forArea(box, '.')
      val resulting = elves.foldLeft(field) { case (acc, e) =>
        acc.updatedAtUnsafe(e.location, e.charRep)
      }
      Field2D.printCharField(resulting)
    }
  }

  def parse(data: String): State = {
    val field = Field2D.parseBooleanField(data)

    val elfPositions = field.filterCoordsByValue(_ == true)
    val elves        = elfPositions.zipWithIndex.map { case (c, idx) =>
      Elf(idx, c)
    }

    State(elves)
  }

  def part1(data: State, rounds: Int): Int = {
    val result = Simulation.runNIterations(data, rounds) {
      case (state, iteration) =>
        state.next(iteration.toInt)
    }

    val box = Coords2D.boundingBoxInclusive(result.allCoords)
    (box.width * box.height) - result.allCoords.size
  }

  def part2(data: State): Long = {
    val (_, lastIteration) = Simulation.runUntilStableState(data) {
      case (state, iteration) =>
        state.next(iteration.toInt)
    }

    lastIteration
  }

  def main(args: Array[String]): Unit = {
    val testData2 =
      """..............
        |..............
        |.......#......
        |.....###.#....
        |...#...#.#....
        |....#...##....
        |...#.###......
        |...##.#.##....
        |....#..#......
        |..............
        |..............
        |..............""".stripMargin

    val realData = readFileText("2022/23.txt")

    val test2 = parse(testData2)
    val real  = parse(realData)

    part1(test2, 10) shouldEqual 110
    part1(real, 10) shouldEqual 3862

    part2(test2) shouldEqual 20
    part2(real) shouldEqual 913
  }
}
